package org.genericsystem.cv;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.commons.pool2.BasePooledObjectFactory;
import org.apache.commons.pool2.PooledObject;
import org.apache.commons.pool2.impl.DefaultPooledObject;
import org.apache.commons.pool2.impl.GenericObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.text.OCRTesseract;

public class Ocr {

	static {
		NativeLibraryLoader.load();
	}

	private static final String TESSDATA_PATH = "/usr/share/tesseract-ocr/4.00/";
	private static final String TESSDATA_ALT_PATH = System.getenv("TESSDATA_PREFIX");
	private static final String TESSERACT_LANGUAGE = "fra";
	private static final String TESSERACT_WHAR_WHITE_LIST = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<'";
	private static final int TESSERACT_OEM = 1;
	private static final int TESSERACT_PSMODE = 13;

	public static void main(String[] args) {
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(Paths.get(".", "classes"), Files::isDirectory)) {
			for (Path directoryPath : directoryStream) {
				System.out.println("--- Images of class: " + directoryPath);
				try (DirectoryStream<Path> imagesStream = Files.newDirectoryStream(directoryPath, path -> Files.isRegularFile(path) && path.toString().endsWith(".png"))) {
					imagesStream.forEach(path -> {
						System.out.println("------ Image: " + path);
						ocrClassifiedImage(path);
					});
				}
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Internal factory class used to create a pool of {@link OCRTesseract} instances. Otherwise, segmentation fault can occur when the instance of tesseract is shared accross multiple threads.
	 * 
	 * @author Pierrik Lassalas
	 */
	public static class OCRTesseractInstanceFactory extends BasePooledObjectFactory<OCRTesseract> {
		@Override
		public OCRTesseract create() throws Exception {
			OCRTesseract instance = null;
			try {
				// Attempt to load tessdata from the default path (when installed from official repository)
				instance = OCRTesseract.create(Ocr.TESSDATA_PATH, Ocr.TESSERACT_LANGUAGE, Ocr.TESSERACT_WHAR_WHITE_LIST, Ocr.TESSERACT_OEM, Ocr.TESSERACT_PSMODE);
			} catch (Exception e) {
				// If tessdata was not found, attempt to load from the alternate path
				try {
					instance = OCRTesseract.create(Ocr.TESSDATA_ALT_PATH, Ocr.TESSERACT_LANGUAGE, Ocr.TESSERACT_WHAR_WHITE_LIST, Ocr.TESSERACT_OEM, Ocr.TESSERACT_PSMODE);
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

	@Deprecated
	public static void ocrClassifiedImage(Path imagePath) {
		Path imgClassDirectory = imagePath.getParent();
		Path zonesFile = imgClassDirectory.resolve("zones/zones.json");
		Zones zones = null;
		if (zonesFile.toFile().exists()) {
			System.out.println("Precomputed zones found, file: " + zonesFile);
			zones = Zones.load(zonesFile.toFile());
		}
		if (zones == null) {
			ImgClass imgClass = ImgClass.fromDirectory(imgClassDirectory.toString());
			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(15, 3)));
			zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
		}
		for (Zone zone : zones) {
			ZoneScorer scorer = zone.newUnsupervisedScorer(Tools.classImgsStream(imgClassDirectory + "/mask/" + imagePath.getFileName().toString().replace(".png", "")));
			System.out.println("Image " + imagePath + ", found text: " + scorer.getBestText() + " " + Math.floor((scorer.getBestScore() * 10000)) / 100 + "%");
		}
	}

	public static String doWork(Mat mat) {
		return doWork(mat, 0);
	}

	public static String doWork(Mat mat, int minConfidence) {
		// Get the OcrTesseract instance from the pool to prevent multi-threading problems
		GenericObjectPool<OCRTesseract> pool = new GenericObjectPool<>(new OCRTesseractInstanceFactory(), Ocr.getPoolConfig());
		OCRTesseract instance = null;
		String ocrText = null;
		try {
			instance = pool.borrowObject();
			ocrText = instance.run(mat, 50, 1).replace("\n", "").trim();
		} catch (Exception e) {
			throw new RuntimeException("An error has occured during the OCR", e);
		} finally {
			// If the instance was retrieved, return it to the pool
			if (instance != null)
				pool.returnObject(instance);
		}
		return ocrText;
	}

	private static GenericObjectPoolConfig getPoolConfig() {
		GenericObjectPoolConfig config = new GenericObjectPoolConfig();
		config.setMaxTotal(2);
		config.setBlockWhenExhausted(true);
		config.setMaxWaitMillis(30_000);
		return config;
	}
}
