package org.genericsystem.cv.comparator;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
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
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The FillModelWithData class can analyze an image (or a batch of images) and
 * store all the OCR text for each zone and each document in GS.
 * 
 * @author Pierrik Lassalas
 *
 */
public class FillModelWithData {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		System.out.println("OpenCV core library loaded");
	}

	public static final int ERROR = 0;
	public static final int NEW_FILE = 1;
	public static final int KNOWN_FILE = 2;
	public static final int KNOWN_FILE_UPDATED_FILTERS = 3;

	private static Logger log = LoggerFactory.getLogger(FillModelWithData.class);
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static final String docType = "id-fr-front";

	public static void main(String[] mainArgs) {
		final Engine engine = new Engine(gsPath, Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class,
			ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class,
			MeanLevenshtein.class, Score.class);
		engine.newCache().start();
		compute(engine);
		// cleanModel(engine);
		engine.close();
	}

	/**
	 * This Map will contain the names of the filters that will be applied to a
	 * specified {@link Img}.
	 * 
	 * @return - a Map containing the filter names as key, and a
	 *         {@link Function} that will apply the specified algorithm to an
	 *         Img.
	 */
	public static Map<String, Function<Img, Img>> getFiltersMap() {
		final Map<String, Function<Img, Img>> map = new ConcurrentHashMap<>();
		map.put("original", i -> i);
		map.put("reality", i -> i);
//		map.put("bernsen", Img::bernsen);
		map.put("equalizeHisto", Img::equalizeHisto);
		map.put("equalizeHistoAdaptative", Img::equalizeHistoAdaptative);
		map.put("otsuAfterGaussianBlur", Img::otsuAfterGaussianBlur);
		map.put("adaptativeGaussianThreshold", i -> i.adaptativeGaussianThreshold());
		return map;
	}

	/**
	 * Check if a given file has already been processed by the system.
	 * 
	 * @param engine
	 *            - the engine used to store the data
	 * @param file
	 *            - the desired file
	 * @return - true if the file was found in the engine, false otherwise
	 */
	private static boolean isFileAlreadyProcessed(Root engine, File file) {
		Generic doc = engine.find(Doc.class);
		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.getDocClass(docType);
		String filename;
		try {
			filename = ModelTools.getHashFromFile(file.toPath(), "sha-256");
		} catch (RuntimeException e) {
			log.error("An error has occured during the generation of the hascode from file (assuming false)", e);
			return false;
		}
		String filenameExt = filename + "." + FilenameUtils.getExtension(file.getName());
		DocInstance docInstance = docClassInstance.getDoc(doc, filenameExt);
		return null != docInstance ? true : false;
	}

	/**
	 * Process an image, and store all the informations in the engine of Generic
	 * System. When no Engine is provided, a default one is created.
	 * 
	 * @param imagePath
	 *            - a {@link Path} object pointing to the image to be processed
	 * @return an {@code int} representing {@link #KNOWN_FILE_UPDATED_FILTERS},
	 *         {@link #NEW_FILE} or {@link #KNOWN_FILE}
	 */
	public static int doImgOcr(Path imagePath) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class,
				Score.class, MeanLevenshtein.class);
		engine.newCache().start();
		int result = doImgOcr(engine, imagePath);
		engine.close();
		return result;
	}

	/**
	 * Process an image, and store all the informations in the engine of Generic
	 * System.
	 * 
	 * @param engine
	 *            - the engine used to store the data
	 * @param imagePath
	 *            - a {@link Path} object pointing to the image to be processed
	 * @return an {@code int} representing {@link #KNOWN_FILE_UPDATED_FILTERS},
	 *         {@link #NEW_FILE} or {@link #KNOWN_FILE}
	 */
	public static int doImgOcr(Root engine, Path imagePath) {

		final Path imgClassDirectory = imagePath.getParent();
		final String docType = imgClassDirectory.getName(imgClassDirectory.getNameCount() - 1).toString();
		// TODO: remove the following line (only present in development)
		final String imgDirectory = imgClassDirectory.toString() + "/ref2/";
		log.info("imgDirectory = {} ", imgDirectory);

		int result;

		// Find and save the doc class
		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.setDocClass(docType);
		engine.getCurrentCache().flush();

		// Get the filters and the predefined zones
		final Map<String, Function<Img, Img>> imgFilters = getFiltersMap();
		final Zones zones = Zones.loadZones(imgClassDirectory.toString());

		// Process the image file
		File file = new File(imagePath.toString());

		initComputation(engine, docType, zones);
		result = processFile(engine, file, docClassInstance, zones, imgFilters.entrySet().stream());
		return result;
	}

	/**
	 * Process all the images in the specified folder, and store all the data in
	 * Generic System.
	 * 
	 * @param engine
	 *            - the engine used to store the data
	 */
	private static void compute(Root engine) {
		final String imgClassDirectory = "classes/" + docType;
		final String imgDirectory = imgClassDirectory + "/ref2/";
		log.info("imgClassDirectory = {} ", imgClassDirectory);
		// Save the current document class
		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.setDocClass(docType);
		// Set all the filter names
		final Map<String, Function<Img, Img>> imgFilters = getFiltersMap();
		// Load the accurate zones
		final Zones zones = Zones.loadZones(imgClassDirectory);
		initComputation(engine, docType, zones);
		// Process each file in folder imgDirectory
		Arrays.asList(new File(imgDirectory).listFiles((dir, name) -> name.endsWith(".png"))).forEach(file -> {
			processFile(engine, file, docClassInstance, zones, imgFilters.entrySet().stream());
			engine.getCurrentCache().flush();
		});
		engine.getCurrentCache().flush();
	}

	/**
	 * Initialize the computation.
	 * 
	 * The zones are added to the model only if they differ from the ones
	 * previously saved. Similarly, the imgFilters Map is analyzed, and a new
	 * Map containing the new filters is returned.
	 * 
	 * This method is used both by {@link #compute(Root)} and
	 * {@link #doImgOcr(Path)}.
	 * 
	 * @param engine
	 *            - the engine used to store the data
	 * @param docType
	 *            - the document type (i.e., class)
	 * @param zones
	 *            - a {@link Zones} object, representing all the zones detected
	 *            for ocr
	 */
	// TODO: change method's name
	private static void initComputation(Root engine, String docType, Zones zones) {

		DocClass docClass = engine.find(DocClass.class);
		DocClassInstance docClassInstance = docClass.getDocClass(docType);
		// Save the zones if necessary
		// TODO: refactor the code (duplicate)
		zones.getZones().forEach(z -> {
			ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
			if (zoneInstance != null) {
				Zone zone = zoneInstance.getZoneObject();
				// log.info("z : {} ; zone : {}", z, zone);
				if (z.equals(zone)) {
					log.info("Zone n°{} already known", z.getNum());
				} else {
					log.info("Adding zone n°{} ", z.getNum());
					docClassInstance.setZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width,
							z.getRect().height);
				}
			} else {
				log.info("Adding zone n°{} ", z.getNum());
				docClassInstance.setZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width,
						z.getRect().height);
			}
		});
		// Persist the changes
		engine.getCurrentCache().flush();
	}

	/**
	 * Process an image file.
	 * 
	 * Each zone of each image is analyzed through OCR, and the results are
	 * stored in Generic System engine.
	 * 
	 * @param engine
	 *            - the engine where the data will be stored
	 * @param file
	 *            - the file to be processed
	 * @param docClassInstance
	 *            - the instance of {@link DocClass} representing the current
	 *            class of the file
	 * @param zones
	 *            - the list of zones for this image
	 * @param imgFilters
	 *            - a stream of entry for a Map containing the filternames that
	 *            will be applied to the original file, and the functions
	 *            required to apply these filters
	 * @return an {@code int} representing {@link #KNOWN_FILE_UPDATED_FILTERS},
	 *         {@link #NEW_FILE} or {@link #KNOWN_FILE}
	 */
	private static int processFile(Root engine, File file, DocClassInstance docClassInstance, Zones zones,
			Stream<Entry<String, Function<Img, Img>>> imgFilters) {

		int result = ERROR;

		log.info("\nProcessing file: {}", file.getName());
		Generic doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneTimestamp zoneTimestamp = engine.find(ZoneTimestamp.class);

		// Save the current file
		String filename = ModelTools.getHashFromFile(file.toPath(), "sha-256");
		String filenameExt = filename + "." + FilenameUtils.getExtension(file.getName());
		log.info("Hash generated for file {}: {}", file.getName(), filenameExt);

		DocInstance docInstance = docClassInstance.setDoc(doc, filenameExt);
		docInstance.setDocFilename(file.getName());
		engine.getCurrentCache().flush();

		// TODO: refactor the code (duplicates)
		// Save the filternames if necessary
		Map<String, Function<Img, Img>> updatedImgFilters = new ConcurrentHashMap<>();
		imgFilters.forEach(entry -> {
			ImgFilterInstance filter = imgFilter.getImgFilter(entry.getKey());
			if (filter == null) {
				log.info("Adding algorithm : {} ", entry.getKey());
				imgFilter.setImgFilter(entry.getKey());
				updatedImgFilters.put(entry.getKey(), entry.getValue());
			} else {
				// TODO: add another criteria to verify if the filter has been
				// applied on the image
				boolean containsNull = zones.getZones().stream().anyMatch(z -> {
					ZoneTextInstance zti = ((ZoneText) engine.find(ZoneText.class)).getZoneText(docInstance,
							docClassInstance.getZone(z.getNum()), filter);
					return zti == null;
				});
				if (containsNull) {
					imgFilter.setImgFilter(entry.getKey());
					updatedImgFilters.put(entry.getKey(), entry.getValue());
				} else {
					log.debug("Algorithm {} already known", entry.getKey());
				}
			}
		});

		// Check whether or not the file has aldready been stored in the system
		if (isFileAlreadyProcessed(engine, file)) {
			if (updatedImgFilters.isEmpty()) {
				log.info("The image {} has already been processed (pass)", file.getName());
				result = KNOWN_FILE;
				return result;
			} else {
				log.info("New filters detected for image {} ", file.getName());
				result = KNOWN_FILE_UPDATED_FILTERS;
			}
		} else {
			log.info("Adding a new image ({}) ", file.getName());
			result = NEW_FILE;
		}

		// Create a map of Imgs
		Img originalImg = new Img(file.getPath());
		Map<String, Img> imgs = new ConcurrentHashMap<>();
		updatedImgFilters.entrySet().forEach(entry -> {
			log.info("Applying algorithm {}...", entry.getKey());
			Img img = null;
			if ("original".equals(entry.getKey()) || "reality".equals(entry.getKey()))
				img = originalImg;
			else
				img = entry.getValue().apply(originalImg);
			if (null != img)
				imgs.put(entry.getKey(), img);
			else
				log.error("An error as occured for image {} and filter {}", filenameExt, entry.getKey());
		});

		// Draw the image's zones + numbers
		Img imgCopy = new Img(file.getPath());
		zones.draw(imgCopy, new Scalar(0, 255, 0), 3);
		zones.writeNum(imgCopy, new Scalar(0, 0, 255), 3);
		// Copy the images to the resources folder
		// TODO implement a filter mechanism to avoid creating
		// duplicates in a public folder
		log.info("Copying {} to resources folder", filenameExt);
		Imgcodecs.imwrite(System.getProperty("user.dir") + "/../gs-cv/src/main/resources/" + filenameExt,
				imgCopy.getSrc());

		// Process each zone
		zones.getZones().forEach(z -> {
			log.info("Zone n° {}", z.getNum());
			ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
			imgs.entrySet().forEach(entry -> {
				if ("reality".equals(entry.getKey()) || "best".equals(entry.getKey())) {
					// Do not proceed to OCR if the real values are known
					// By default, the "reality" and "best" filters are left
					// empty
					if (null == zoneText.getZoneText(docInstance, zoneInstance, imgFilter.getImgFilter(entry.getKey())))
						zoneText.setZoneText("", docInstance, zoneInstance, imgFilter.getImgFilter(entry.getKey()));
				} else {
					String ocrText = z.ocr(entry.getValue());
					ZoneTextInstance zti = zoneText.setZoneText(ocrText.trim(), docInstance, zoneInstance,
							imgFilter.getImgFilter(entry.getKey()));
					zoneTimestamp.setZoneTimestamp(ModelTools.getCurrentDate(), zti); // TODO: test
				}
			});
			engine.getCurrentCache().flush();
		});

		return result;
	}

	private static Map<String, Function<Img, Img>> filterOptimizationMap() {
		final Map<String, Function<Img, Img>> imgFilters = new ConcurrentHashMap<>();
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
				imgFilters.put("nick" + "_" + bs + "_" + k.toString().replace("-", "m"),
						img -> img.niblackThreshold(bs, k));
			}
		}
		imgFilters.put("reality", i -> i);
		imgFilters.put("original", i -> i);
		return imgFilters;
	}

	/**
	 * Remove all the data stored in the engine, except the real values used for
	 * training (e.g., for which imgFilter = "reality")
	 * 
	 * @param engine
	 *            - the engine used to store the data
	 */
	@SuppressWarnings({ "unused", "unchecked", "rawtypes" })
	private static void cleanModel(Root engine) {

		System.out.println("Cleaning model...");

		final String imgClassDirectory = "classes/" + docType;
		final String imgDirectory = imgClassDirectory + "/ref2/";

		// Get the necessary classes from the engine
		DocClass docClass = engine.find(DocClass.class);
		Generic doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein ml = engine.find(MeanLevenshtein.class);

		// Save the current document class
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);

		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances()
				.filter(f -> !"reality".equals(f.getValue())).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();

		docInstances.forEach(currentDoc -> {
			imgFilterInstances.forEach(i -> {
				zoneInstances.forEach(z -> {
					ZoneTextInstance zti = zoneText.getZoneText(currentDoc, z, i);
					if (zti != null)
						zti.remove();
				});
				engine.getCurrentCache().flush();
			});
		});

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
		engine.getCurrentCache().flush();

		System.out.println("Done!");
	}
}
