package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;
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
import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.genericsystem.cv.utils.OCRPlasty.Tuple;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.text.OCRTesseract;

public class Ocr {

	static {
		NativeLibraryLoader.load();
	}

	// Get the OcrTesseract instance from the tesseractInstancePool to prevent multi-threading problems
	private static final GenericObjectPool<OCRTesseract> tesseractInstancePool = new GenericObjectPool<>(new OCRTesseractInstanceFactory(), Ocr.buildPoolConfig());

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

		int labelsSize = field.getLabelsSize();
		if (labelsSize >= MIN_SIZE_CONSOLIDATION) {
			List<String> strings = field.getLabels().entrySet().stream().collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())), List::addAll);
			Tuple res = OCRPlasty.correctStringsAndGetOutliers(strings, RANSAC.NORM_LEVENSHTEIN);			
			field.setConsolidated(res.getString());			
			field.setConfidence(res.getConfidence());
			//			if (labelsSize >= 2 * MIN_SIZE_CONSOLIDATION)
			//				res.getOutliers().forEach(outlier -> field.getLabels().remove(outlier));
		} else {
			//logger.trace("Not enough labels to consolidate OCR (current minimum = {})", MIN_SIZE_CONSOLIDATION);
			field.setConsolidated(null);
			field.setConfidence(0);
		}
		field.adjustLockLevel(field.getConfidence() > CONFIDENCE_THRESHOLD ? 1 : -0.5);
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
