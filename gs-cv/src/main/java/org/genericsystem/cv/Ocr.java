package org.genericsystem.cv;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.pool2.BasePooledObjectFactory;
import org.apache.commons.pool2.PooledObject;
import org.apache.commons.pool2.impl.DefaultPooledObject;
import org.apache.commons.pool2.impl.GenericObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.genericsystem.cv.retriever.Field;
import org.genericsystem.cv.retriever.Stats;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.OCRPlasty.OcrModel;
import org.genericsystem.cv.utils.Ransac;
import org.genericsystem.cv.utils.Ransac.Model;
import org.genericsystem.reinforcer.tools.Levenshtein;
import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.text.OCRTesseract;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Ocr {

	static {
		NativeLibraryLoader.load();
	}

	// Get the OcrTesseract instance from the tesseractInstancePool to prevent multi-threading problems
	private static final GenericObjectPool<OCRTesseract> tesseractInstancePool = new GenericObjectPool<>(new OCRTesseractInstanceFactory(), Ocr.buildPoolConfig());

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final String TESSDATA_PATH = "/usr/share/tesseract-ocr/4.00/";
	private static final String TESSDATA_ALT_PATH = System.getenv("TESSDATA_PREFIX");
	private static final String TESSERACT_LANGUAGE = "fra";
	private static final String TESSERACT_CHAR_WHITE_LIST = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<>!?;éèàçÉÈÀÇ€£$œ'";
	private static final int TESSERACT_OEM = 1;
	private static final int TESSERACT_PSMODE = 13;

	private static final double CONFIDENCE_THRESHOLD = 0.92;
	protected static final int MIN_SIZE_CONSOLIDATION = 5;
	private static final int OCR_CONFIDENCE_THRESH = 0;

	/**
	 * Internal factory class used to create a tesseractInstancePool of {@link OCRTesseract} instances. Otherwise, segmentation fault can occur when the instance of tesseract is shared accross multiple threads.
	 */
	public static class OCRTesseractInstanceFactory extends BasePooledObjectFactory<OCRTesseract> {
		@Override
		public OCRTesseract create() throws Exception {
			OCRTesseract instance = null;
			try {
				// Attempt to load tessdata from the default path (when installed from official repository)
				instance = OCRTesseract.create(Ocr.TESSDATA_PATH, Ocr.TESSERACT_LANGUAGE, Ocr.TESSERACT_CHAR_WHITE_LIST, Ocr.TESSERACT_OEM, Ocr.TESSERACT_PSMODE);
			} catch (Exception e) {
				// If tessdata was not found, attempt to load from the alternate path
				try {
					instance = OCRTesseract.create(Ocr.TESSDATA_ALT_PATH, Ocr.TESSERACT_LANGUAGE, Ocr.TESSERACT_CHAR_WHITE_LIST, Ocr.TESSERACT_OEM, Ocr.TESSERACT_PSMODE);
				} catch (Exception e1) {
					throw new RuntimeException("Unable to load tesseract data. Please ensure that tesseract-ocr is installed and configured properly on your system.", e);
				}
			}
			return instance;
		}

		@Override
		public PooledObject<OCRTesseract> wrap(OCRTesseract instance) {
			return new DefaultPooledObject<>(instance);
		}
	}


	public static String performOcr(Img rootImg, Field f) {
		int attempts = f.getAttempts();
		if (f.getOcrRect().isNearEdge(rootImg.width(), rootImg.height(), 10))
			return null;
		Rect rect = new Rect((int) f.getOcrRect().getX(), (int) f.getOcrRect().getY(), (int) f.getOcrRect().getWidth(), (int) f.getOcrRect().getHeight());	
		String ocr = doWork(new Mat(rootImg.getSrc(), rect), OCR_CONFIDENCE_THRESH);
		if (ocr.length()!=0) {
			f.getLabels().merge(ocr, 1, Integer::sum);
			f.setAttempts(++attempts);
		}
		if (attempts <= 3 || attempts % 5 == 0) {
			Stats.beginTask("ocr plasty");
			consolidateOcr(f);
			Stats.endTask("ocr plasty");
		}
		return ocr;
	}

	private static void consolidateOcr(Field field) {		
		if (field.getLabelsSize() >= MIN_SIZE_CONSOLIDATION) {
			//get the exhaustive list of OCRs: the same ocr should appear twice if it was discovered two times.  
			List<String> trimmed = trimLabels(field.getLabels().entrySet().stream().collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())), List::addAll));
			List<String> inliers = getLabelRansac(trimmed, 0.1).getBestDataSet().values().stream().collect(Collectors.toList());
			field.setConfidence(getConfidence(inliers, StringCompare.SIMILARITY.LEVENSHTEIN));	
			field.setConsolidated(inliers.isEmpty() ? OCRPlasty.ocrPlasty(trimmed) : OCRPlasty.ocrPlasty(inliers));
		} 
		field.adjustLockLevel(field.getConfidence() > CONFIDENCE_THRESHOLD ? 1 : -0.5);
	}

	private static Ransac<String> getLabelRansac(List<String> labels, double error){
		int minSize = 1 + labels.size() / 2;
		if (minSize < 2)
			return null;
		Ransac<String> ransac = null;		
		for (int i = 1;  i <= 10; ++i) {
			try {
				ransac = new Ransac<>(labels, getModelProviderNormLevenshtein(), 2, 10 * i, error, minSize);
			} catch (Exception e) {
				error *= 1.5;
				logger.trace("Can't get a good model. Increase the error margin to {}", error);
			}
		}
		return ransac;
	}

	private static Function<Collection<String>, Model<String>> getModelProviderNormLevenshtein() {
		return datas -> {
			return new OcrModel() {
				@Override
				public double computeError(String data) {
					error = 0d;
					for (String s : datas) {
						error += Levenshtein.normedDistance(data, s);
					}
					return error / datas.size();
				}
			};
		};
	}


	private static List<String> trimLabels(List<String> labels) {
		return labels.stream().map(s -> s.trim()).filter(s -> s.length() > 0).collect(Collectors.toList());
	}

	private static double getConfidence(List<String> inliers, SIMILARITY method) {
		return StringCompare.similarity(inliers, method);
	}

	public static String doWork(Mat mat) {
		return doWork(mat, 0);
	}

	public static String doWork(Mat mat, int minConfidence) {
		OCRTesseract instance = null;
		String ocrText = null;
		try {
			instance = tesseractInstancePool.borrowObject();
			ocrText = instance.run(mat, minConfidence, 1).replace("\n", "").trim();
		} catch (Exception e) {
			throw new RuntimeException("An error has occured during the OCR", e);
		} finally {
			// If the instance was retrieved, return it to the tesseractInstancePool
			if (instance != null)
				tesseractInstancePool.returnObject(instance);
		}
		return ocrText;
	}

	private static GenericObjectPoolConfig buildPoolConfig() {
		GenericObjectPoolConfig config = new GenericObjectPoolConfig();
		config.setMaxTotal(Runtime.getRuntime().availableProcessors());
		config.setBlockWhenExhausted(true);
		config.setMaxWaitMillis(30_000);
		return config;
	}
}
