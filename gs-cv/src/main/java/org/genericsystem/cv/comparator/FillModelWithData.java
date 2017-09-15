package org.genericsystem.cv.comparator;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zone;
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
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
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
		compute(engine);
		// cleanModel(engine);
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
		return new Engine(gsPath, Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class, MeanLevenshtein.class, Score.class);
	}

	/**
	 * This List contains all the functions defined in {@link ImgFilterFunction}.
	 * 
	 * @return a list of {@link ImgFilterFunction}
	 */
	public static List<ImgFilterFunction> getFilterFunctions() {
		final List<ImgFilterFunction> filterSet = new ArrayList<>();
		for (ImgFilterFunction iff : ImgFilterFunction.values()) {
			logger.info("Adding: {}", iff);
			filterSet.add(iff);
		}
		return filterSet;
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
	public static JsonObject getOcrParameters(Root engine, Path imagePath) {
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
		engine.getCurrentCache().flush();

		// Get the filters and the predefined zones
		final List<ImgFilterFunction> imgFilterFunctions = FillModelWithData.getFilterFunctions();
		final Zones zones = Zones.load(imgClassDirectory.toString());

		// Save the zones if necessary
		zones.getZones().forEach(z -> {
			ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
			if (zoneInstance != null) {
				Zone zone = zoneInstance.getZoneObject();
				// logger.info("z : {} ; zone : {}", z, zone);
				if (z.equals(zone)) {
					logger.info("Zone n°{} already known", z.getNum());
				} else {
					logger.info("Adding zone n°{} ", z.getNum());
					docClassInstance.setZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width, z.getRect().height);
				}
			} else {
				logger.info("Adding zone n°{} ", z.getNum());
				docClassInstance.setZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width, z.getRect().height);
			}
		});
		engine.getCurrentCache().flush();

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
					ZoneTextInstance zti = zoneText.getZoneText(docInstance, docClassInstance.getZone(z.getNum()), filter);
					return zti == null;
				});
				if (containsNullZoneTextInstance) {
					logger.debug("Detected new algorithm ({}) for {}", entry.getName(), docInstance);
					imgFilter.setImgFilter(filtername);
					updatedImgFilterList.add(entry);
				} else {
					logger.debug("Algorithm {} already known", filtername);
				}
			}
		});

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
			result.put(String.valueOf(z.getNum()), map);
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
	 * @param data - a {@link JsonObject} containing all the data (see {@link #getOcrParameters(Root, Path)}).
	 */
	public static void saveOcrDataInModel(Root engine, JsonObject data) {
		// Parse the data
		String docType = data.getString(CLASS_NAME);
		String filename = data.getString(FILENAME);
		String filenameExt = data.getString(ENCODED_FILENAME);
		Long timestamp = data.getLong(DOC_TIMESTAMP);
		JsonObject zones = data.getJsonObject(ZONES);

		// Get the generics
		DocClass docClass = engine.find(DocClass.class);
		Doc doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		try {
			// Set the docClass, doc instance and timestamp
			DocClassInstance docClassInstance = docClass.setDocClass(docType);
			DocInstance docInstance = docClassInstance.setDoc(doc, filenameExt);
			docInstance.setDocFilename(filename);
			docInstance.setDocTimestamp(timestamp);
			engine.getCurrentCache().flush();

			zones.forEach(entry -> {
				logger.info("Current zone: {}", entry.getKey());
				ZoneInstance zoneInstance = docClassInstance.getZone(Integer.parseInt(entry.getKey(), 10));
				JsonObject currentZone = (JsonObject) entry.getValue();
				if (!currentZone.isEmpty())
					currentZone.put("reality", ""); // Add this filter only if there are other filters
				currentZone.forEach(e -> {
					// logger.debug("key: {}; value: {}", e.getKey(), e.getValue());
					if ("reality".equals(e.getKey()) || "best".equals(e.getKey())) {
						// Do not proceed to OCR if the real values are known. By default, the "reality" and "best" filters are left empty
						if (null == zoneText.getZoneText(docInstance, zoneInstance, imgFilter.getImgFilter(e.getKey())))
							zoneText.setZoneText("", docInstance, zoneInstance, imgFilter.getImgFilter(e.getKey()));
					} else {
						String ocrText = (String) e.getValue();
						ZoneTextInstance zti = zoneText.setZoneText(ocrText, docInstance, zoneInstance, imgFilter.getImgFilter(e.getKey()));
						zti.setZoneTimestamp(ModelTools.getCurrentDate()); // TODO: concatenate with previous line?
					}
				});
				engine.getCurrentCache().flush();
			});
			logger.info("Data for {} successfully saved.", filenameExt);
		} catch (Exception e) {
			throw new RuntimeException("An error has occured while saving the OCR data into the Engine", e);
		}
	}

	/**
	 * Process an image, and store all the informations in the engine of Generic System. When no Engine is provided, a default one is created.
	 * 
	 * @param imagePath - a {@link Path} object pointing to the image to be processed
	 * @return an {@code int} representing {@link #KNOWN_FILE_UPDATED_FILTERS}, {@link #NEW_FILE} or {@link #KNOWN_FILE}
	 */
	public static void doImgOcr(Path imagePath) {
		final Root engine = getEngine();
		engine.newCache().start();
		doImgOcr(engine, imagePath);
		engine.close();
	}

	/**
	 * Process an image, and store all the informations in the engine of Generic System.
	 * 
	 * @param engine - the engine used to store the data
	 * @param imagePath - a {@link Path} object pointing to the image to be processed
	 * @return an {@code int} representing {@link #KNOWN_FILE_UPDATED_FILTERS}, {@link #NEW_FILE} or {@link #KNOWN_FILE}
	 */
	public static void doImgOcr(Root engine, Path imagePath) {
		final String docType = ModelTools.getImgClass(imagePath);

		// Find and save the doc class
		DocClass docClass = engine.find(DocClass.class);
		docClass.setDocClass(docType);
		engine.getCurrentCache().flush();

		// Process the image file
		JsonObject params = getOcrParameters(engine, imagePath);
		JsonObject ocrData = processFile(params);
		saveOcrDataInModel(engine, ocrData);
	}

	/**
	 * Process all the images in the specified folder, and store all the data in Generic System. The docType is set to the default value.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public static void compute(Root engine) {
		compute(engine, docType);
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
			JsonObject params = getOcrParameters(engine, file.toPath());
			JsonObject ocrData = processFile(params);
			saveOcrDataInModel(engine, ocrData);
		});
		engine.getCurrentCache().flush();
	}

	/**
	 * Save a new document in Generic System using the default Engine.
	 * 
	 * @param imgPath - the Path of the file
	 * @return true if this was a success, false otherwise
	 */
	public static boolean registerNewFile(Path imgPath) {
		final Root engine = getEngine();
		engine.newCache().start();
		boolean result = registerNewFile(engine, imgPath, System.getProperty("user.dir") + "/../gs-ir/src/main/resources/");
		engine.close();
		return result;
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
				}
				return true;
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

	/**
	 * Remove all the data stored in the engine, except the real values used for training (e.g., for which imgFilter = "reality")
	 * 
	 * @param engine - the engine used to store the data
	 */
	@SuppressWarnings({ "unused", "unchecked", "rawtypes" })
	private static void cleanModel(Root engine) {
		System.out.println("Cleaning model...");
		// Get the necessary classes from the engine
		DocClass docClass = engine.find(DocClass.class);
		Generic doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein ml = engine.find(MeanLevenshtein.class);

		// Save the current document class
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);

		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue())).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();

		// Delete all ZoneTextInstances that are not "reality"
		docInstances.forEach(currentDoc -> {
			imgFilterInstances.forEach(i -> {
				zoneInstances.forEach(z -> {
					ZoneTextInstance zti = zoneText.getZoneText(currentDoc, z, i);
					if (zti != null) {
						zti.getHolders(engine.find(ZoneTimestamp.class)).forEach(g -> g.remove());
						zti.remove();
					}
				});
				engine.getCurrentCache().flush();
			});
		});

		// Delete all filters that are not reality", and their attached scores
		imgFilterInstances.forEach(i -> {
			zoneInstances.forEach(z -> {
				ScoreInstance scoreInst = score.getScore(z, i);
				if (scoreInst != null) {
					scoreInst.getHolder(ml).remove();
					scoreInst.remove();
				}
			});
			i.remove();
			engine.getCurrentCache().flush();
		});

		// Finally delete all documents for which no ZoneTextInstances exist (i.e., not supervised)
		docInstances.forEach(currentDoc -> {
			zoneInstances.forEach(z -> {
				boolean result = imgFilter.getInstances().stream().allMatch(i -> {
					ZoneTextInstance zti = zoneText.getZoneText(currentDoc, z, (ImgFilterInstance) i);
					return null == zti || zti.getValue().toString().isEmpty();
				});
				if (result) {
					currentDoc.getDependencies().forEach(dependency -> {
						currentDoc.getHolders(dependency).forEach(g -> g.remove());
						dependency.remove();
						engine.getCurrentCache().flush();
					});
					// FIXME unable to delete the currentDoc (AliveConstraint violation)
					currentDoc.remove();
					engine.getCurrentCache().flush();
				}
			});
		});

		engine.getCurrentCache().flush();
		System.out.println("Done!");
	}
}
