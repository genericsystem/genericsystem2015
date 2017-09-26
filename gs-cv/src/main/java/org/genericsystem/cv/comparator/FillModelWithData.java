package org.genericsystem.cv.comparator;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocFilename;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.Doc.DocTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.LevDistance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneGeneric.ZoneNum;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.kernel.Engine;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.JsonObject;

/**
 * The FillModelWithData class contains all the methods needed for the OCR of an image. One can process a single image, or a batch of images, and store all the data in Generic System. The OCR and the persistence can be done separately (the necessary
 * informations are transmitted using {@link JsonObject} from vert.x.
 * 
 * @author Pierrik Lassalas
 */
public class FillModelWithData {

	public static final String ENCODED_FILENAME = "encodedFilename";
	public static final String FILENAME = "filename";
	public static final String CLASS_NAME = "docType";
	public static final String DOC_TIMESTAMP = "docTimestamp";
	public static final String ZONES = "zones";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static final String docType = "id-fr-front";

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] mainArgs) {
		final Root engine = getEngine();
		engine.newCache().start();
		compute(engine, docType);
		engine.close();
	}

	/**
	 * Get an Engine using the default path.
	 * 
	 * @return the created Root
	 */
	public static Root getEngine() {
		return getEngine(gsPath);
	}

	/**
	 * Get an Engine using a custom path.
	 * 
	 * @param gsPath - the persistence path
	 * @return the created Root
	 */
	public static Root getEngine(String gsPath) {
		return new Engine(gsPath, Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class, ZoneGeneric.class, ZoneNum.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class, MeanLevenshtein.class,
				Score.class);
	}

	/**
	 * This List contains all the functions defined in {@link ImgFilterFunction}.
	 * 
	 * @return a list of {@link ImgFilterFunction}
	 */
	public static List<ImgFilterFunction> getFilterFunctions() {
		final List<ImgFilterFunction> filterList = new ArrayList<>();
		for (ImgFilterFunction iff : ImgFilterFunction.values()) {
			logger.info("Adding: {}", iff);
			filterList.add(iff);
		}
		return filterList;
	}

	/**
	 * Check if a given file has already been processed by the system. This verification is conducted by comparing the SHA-256 hash code generated from the file and the one stored in Generic System. If there is a match, the file is assumed to be known.
	 * 
	 * @param engine - the engine used to store the data
	 * @param file - the desired file
	 * @param docType - {@code String} representing the type of document (i.e., class)
	 * @return - true if the file was not found in the engine, false if it has already been processed
	 */
	private static boolean isThisANewFile(Root engine, File file, String docType) {
		Generic doc = engine.find(Doc.class);
		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.getDocClass(docType);
		String filenameExt = ModelTools.generateFileName(file.toPath());
		if (null == filenameExt) {
			logger.error("An error has occured during the generation of the hascode from file (assuming new)");
			return true;
		} else {
			DocInstance docInstance = docClassInstance.getDoc(doc, filenameExt);
			return null == docInstance ? true : false;
		}
	}

	/**
	 * Collect all the informations required to process a given file through OCR.
	 * 
	 * @param engine - the engine used to store the data
	 * @param imagePath - the {@link Path} of the image to proceed
	 * @return a {@link JsonObject} containing all the informations required to process the file
	 */
	public static JsonObject buildOcrParameters(Root engine, Path imagePath) {
		final Path imgClassDirectory = imagePath.getParent();
		final String docType = ModelTools.getImgClass(imagePath);

		// Find the generics
		DocClass docClass = engine.find(DocClass.class);
		Doc doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		// Find and save the doc class and the doc instance
		DocClassInstance docClassInstance = docClass.setDocClass(docType);
		DocInstance docInstance = docClassInstance.setDoc(doc, ModelTools.generateFileName(imagePath));

		// Get the filters and the predefined zones
		final List<ImgFilterFunction> imgFilterFunctions = FillModelWithData.getFilterFunctions();
		final Zones zones = Zones.load(imgClassDirectory.toString());

		// Save the zones if necessary
		zones.getZones().forEach(z -> {
			ZoneInstance zoneInstance = docClassInstance.getZone(z.getUid());
			if (zoneInstance != null) {
				if (z.getUid().equals(zoneInstance.getValue())) {
					logger.info("Zone n°{} already known", z.getUid());
				} else {
					logger.info("Adding zone n°{} ", z.getUid());
					docClassInstance.setZone(z);
				}
			} else {
				logger.info("Adding zone n°{} ", z.getUid());
				docClassInstance.setZone(z);
			}
		});

		// Save the filternames if necessary
		final List<ImgFilterFunction> updatedImgFilterList = new ArrayList<>();
		imgFilterFunctions.forEach(entry -> {
			String filtername = entry.getName();
			ImgFilterInstance filter = imgFilter.getImgFilter(filtername);
			if (filter == null) {
				logger.info("Adding algorithm : {} ", filtername);
				imgFilter.setImgFilter(filtername);
				updatedImgFilterList.add(entry);
			} else {
				// TODO: add another criteria to verify if the filter has been applied on the image
				boolean containsNullZoneTextInstance = zones.getZones().stream().anyMatch(z -> {
					ZoneTextInstance zti = zoneText.getZoneText(docInstance, docClassInstance.getZone(z.getUid()), filter);
					return zti == null;
				});
				if (containsNullZoneTextInstance) {
					logger.debug("Detected new algorithm ({}) for {}", filtername, docInstance);
					imgFilter.setImgFilter(filtername);
					updatedImgFilterList.add(entry);
				} else {
					logger.debug("Algorithm {} already known", filtername);
				}
			}
		});

		engine.getCurrentCache().flush(); // XXX move to somewhere else?

		if (null == updatedImgFilterList || updatedImgFilterList.isEmpty()) {
			logger.info("Nothing to add");
			return new JsonObject();
		} else {
			// Return the parameters required to process this file as a JsonObject
			OcrParameters params = new OcrParameters(imagePath.toFile(), zones, updatedImgFilterList);
			return params.toJson();
		}
	}

	/**
	 * Process a given file through OCR. All the necessary parameters are retrieved from the {@code params} argument. The results are stored in a {@link JsonObject}.
	 * 
	 * @param params - the {@link OcrParameters} as a {@link JsonObject}
	 * @return a {@link JsonObject} containing all the data from the OCR
	 */
	public static JsonObject processFile(JsonObject params) {
		// Get all necessary parameters from the JsonObject
		OcrParameters ocrParameters = new OcrParameters(params);
		File file = ocrParameters.getFile();
		Zones zones = ocrParameters.getZones();
		List<ImgFilterFunction> updatedImgFilterList = ocrParameters.getImgFilterFunctions();

		// Save the current file
		logger.info("\nProcessing file: {}", file.getName());
		String filenameExt = ModelTools.generateFileName(file.toPath());
		if (null == filenameExt)
			throw new RuntimeException("An error has occured while saving the file! Aborted...");
		final Path imgClassDirectory = file.toPath().getParent();
		final String docType = imgClassDirectory.getName(imgClassDirectory.getNameCount() - 1).toString();

		// Create a JsonObject for the answer
		JsonObject jsonObject = new JsonObject();
		jsonObject.put(CLASS_NAME, docType);
		jsonObject.put(FILENAME, file.getName());
		jsonObject.put(ENCODED_FILENAME, filenameExt);
		jsonObject.put(DOC_TIMESTAMP, ModelTools.getCurrentDate());

		// Create a map of Imgs
		Map<String, Img> imgs = new ConcurrentHashMap<>();
		Img originalImg = new Img(file.getPath());
		updatedImgFilterList.forEach(entry -> {
			String filtername = entry.getName();
			ImgFunction function = entry.getLambda();
			logger.info("Applying algorithm {}...", filtername);
			Img img = null;
			if ("original".equals(filtername) || "reality".equals(filtername))
				img = originalImg;
			else
				img = function.apply(originalImg);
			if (null != img)
				imgs.put(filtername, img);
			else
				logger.error("An error as occured for image {} and filter {}", filenameExt, filtername);
		});

		// Process each zone
		Map<String, Map<String, String>> result = new ConcurrentHashMap<>();
		zones.getZones().forEach(z -> {
			logger.info("Zone n° {}", z.getNum());
			Map<String, String> map = new ConcurrentHashMap<>();
			imgs.entrySet().forEach(entry -> {
				if ("reality".equals(entry.getKey()) || "best".equals(entry.getKey())) {
					// Do nothing
				} else {
					String ocrText = z.ocr(entry.getValue());
					map.put(entry.getKey(), ocrText);
				}
			});
			result.put(String.valueOf(z.getUid()), map);
		});
		jsonObject.put(ZONES, result);

		// Close the images to force freeing OpenCV's resources (native matrices)
		originalImg.close();
		imgs.entrySet().forEach(entry -> entry.getValue().close());

		return jsonObject;
	}

	/**
	 * Save the OCR data into Generic System.
	 * 
	 * @param engine - the engine used to store the data
	 * @param data - a {@link JsonObject} containing all the data (see {@link #buildOcrParameters(Root, Path)}).
	 */
	public static void saveOcrDataInModel(Root engine, JsonObject data) {
		// Parse the data
		String docType = data.getString(CLASS_NAME);
		String filename = data.getString(FILENAME);
		String filenameExt = data.getString(ENCODED_FILENAME);
		Long timestamp = data.getLong(DOC_TIMESTAMP);
		JsonObject zonesResults = data.getJsonObject(ZONES);

		// Get the generics
		DocClass docClass = engine.find(DocClass.class);
		Doc doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		try {
			// Set the docClass, doc instance and timestamp
			DocClassInstance docClassInstance = docClass.setDocClass(docType);
			DocInstance docInstance = docClassInstance.setDoc(doc, filenameExt);
			try {
				docInstance.setDocFilename(filename);
				docInstance.setDocTimestamp(timestamp);
			} catch (RollbackException e1) {
				logger.debug("Filename or timestamp have already been set. Resuming task...");
			} catch (Exception e1) {
				throw new RuntimeException(e1);
			}

			zonesResults.forEach(entry -> {
				logger.info("Current zone: {}", entry.getKey()); // Prints the UID
				ZoneInstance zoneInstance = docClassInstance.getZone(String.valueOf(entry.getKey()));
				JsonObject currentZone = (JsonObject) entry.getValue();
				if (!currentZone.isEmpty())
					currentZone.put("reality", ""); // Add this filter only if there are other filters
				currentZone.forEach(e -> {
					logger.trace("key: {}; value: {}", e.getKey(), e.getValue());
					ImgFilterInstance imgFilterInstance = imgFilter.getImgFilter(e.getKey());
					if (null == imgFilterInstance)
						throw new NullPointerException("Cannot retrieve imgFilterInstance from the Engine");

					if ("reality".equals(e.getKey()) || "best".equals(e.getKey())) {
						// Do not proceed to OCR if the real values are known. By default, the "reality" and "best" filters are left empty
						if (null == zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance))
							zoneText.setZoneText("", docInstance, zoneInstance, imgFilterInstance);
					} else {
						String ocrText = (String) e.getValue();
						zoneText.setZoneText(ocrText, docInstance, zoneInstance, imgFilterInstance).setZoneTimestamp(ModelTools.getCurrentDate());
					}
				});
			});

			engine.getCurrentCache().flush();
			logger.info("Data for {} successfully saved.", filenameExt);
		} catch (Exception e) {
			throw new RuntimeException("An error has occured while saving the OCR data into the Engine", e);
		}
	}

	/**
	 * Process all the images in the specified folder, and store all the data in Generic System.
	 * 
	 * @param engine - the engine used to store the data
	 * @param docType - {@code String} representing the type of document (i.e., class)
	 */
	public static void compute(Root engine, String docType) {
		final String imgClassDirectory = "classes/" + docType;
		logger.debug("imgClassDirectory = {} ", imgClassDirectory);
		DocClass docClass = engine.find(DocClass.class);
		docClass.setDocClass(docType);

		Arrays.asList(new File(imgClassDirectory).listFiles((dir, name) -> name.endsWith(".png"))).forEach(file -> {
			JsonObject params = buildOcrParameters(engine, file.toPath());
			JsonObject ocrData = processFile(params);
			saveOcrDataInModel(engine, ocrData);
			engine.getCurrentCache().flush();
		});
		engine.getCurrentCache().flush();
	}

	/**
	 * Save a new document in Generic System.
	 * 
	 * @param imgPath - the Path of the file
	 * @return true if this was a success, false otherwise
	 */
	public static boolean registerNewFile(Root engine, Path imgPath, String resourcesFolder) {
		final String docType = ModelTools.getImgClass(imgPath);

		// Find and save the doc class
		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.setDocClass(docType);
		engine.getCurrentCache().flush();

		final boolean newFile = isThisANewFile(engine, imgPath.toFile(), docType);
		if (!newFile) {
			logger.info("Image {} is already known", imgPath.getFileName());
			return true;
		} else {
			logger.info("Adding a new image ({}) ", imgPath.getFileName());
			String filenameExt = ModelTools.generateFileName(imgPath);
			Generic doc = engine.find(Doc.class);
			DocInstance docInstance = docClassInstance.setDoc(doc, filenameExt);
			if (null != docInstance) {
				docInstance.setDocFilename(imgPath.getFileName().toString());
				docInstance.setDocTimestamp(ModelTools.getCurrentDate());
				engine.getCurrentCache().flush();
				try (Img img = new Img(imgPath.toString())) {
					logger.info("Copying {} to resources folder", filenameExt);
					Imgcodecs.imwrite(resourcesFolder + filenameExt, img.getSrc());
				} catch (Exception e) {
					logger.error("An error has occured while saving file into resources folder " + filenameExt, e);
				}
				return true; // Even if the file was not copied to the resources folder, it has still been correctly processed
			} else {
				logger.error("An error has occured while saving file {}", filenameExt);
				return false;
			}
		}
	}

	@SuppressWarnings("unused")
	private static Map<String, ImgFunction> filterOptimizationMap() {
		final Map<String, ImgFunction> imgFilters = new ConcurrentHashMap<>();
		// Niblack
		// List<Integer> blockSizes = Arrays.asList(new Integer[] { 7, 9, 11,
		// 15, 17, 21, 27, 37 });
		// List<Double> ks = Arrays.asList(new Double[] { -1.0, -0.8, -0.6,
		// -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1 });
		// Sauvola, Nick
		List<Integer> blockSizes = Arrays.asList(new Integer[] { 7, 9, 11, 15, 17, 21, 27, 37 });
		List<Double> ks = Arrays.asList(new Double[] { 0.0, 0.1, 0.2, 0.3, 0.4 });
		// Wolf
		// List<Integer> blockSizes = Arrays.asList(new Integer[] { 7, 9, 11,
		// 15, 17, 21, 27, 37 });
		// List<Double> ks = Arrays.asList(new Double[] { -0.25, -0.2, -0.15,
		// 0.1, -0.05, 0.0 });
		for (Integer bs : blockSizes) {
			for (Double k : ks) {
				imgFilters.put("nick" + "_" + bs + "_" + k.toString().replace("-", "m"), img -> img.niblackThreshold(bs, k));
			}
		}
		imgFilters.put("reality", i -> i);
		imgFilters.put("original", i -> i);
		return imgFilters;
	}

}
