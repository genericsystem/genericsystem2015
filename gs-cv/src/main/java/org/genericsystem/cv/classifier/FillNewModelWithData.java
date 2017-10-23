package org.genericsystem.cv.classifier;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.cv.comparator.ImgFilterFunction;
import org.genericsystem.cv.comparator.ImgFunction;
import org.genericsystem.cv.newmodel.SimpleModel.ConsolidatedType;
import org.genericsystem.cv.newmodel.SimpleModel.DocClassType;
import org.genericsystem.cv.newmodel.SimpleModel.DocClassType.DocClassInstance;
import org.genericsystem.cv.newmodel.SimpleModel.DocType;
import org.genericsystem.cv.newmodel.SimpleModel.DocType.DocInstance;
import org.genericsystem.cv.newmodel.SimpleModel.ImgDocRel;
import org.genericsystem.cv.newmodel.SimpleModel.ImgDocRel.ImgDocLink;
import org.genericsystem.cv.newmodel.SimpleModel.ImgPathType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgRefreshTimestampType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgTimestampType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.newmodel.SimpleModel.LayoutType;
import org.genericsystem.cv.newmodel.SimpleModel.SupervisedType;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneNumType;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneType;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneType.ZoneInstance;
import org.genericsystem.cv.utils.ClassifierUsingFields;
import org.genericsystem.cv.utils.Deskewer;
import org.genericsystem.cv.utils.Deskewer.METHOD;
import org.genericsystem.cv.utils.ModelTools;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Rect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.JsonObject;

public class FillNewModelWithData {

	static {
		NativeLibraryLoader.load();
	}

	private static final String DOC_PATH = "docPath";
	private static final String FILENAME = "filename";
	private static final String ENCODED_FILENAME = "encodedFilename";
	private static final String DOC_TIMESTAMP = "docTimestamp";
	private static final String ZONES = "zones";
	private static final String FIELD_NUM = "fieldNum";
	private static final String RECT = "rectangle";
	private static final String CONSOLIDATED = "consolidated";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv-newmodel";

	public static void main(String[] args) {
		Path basePath = Paths.get(System.getenv("HOME") + "/genericsystem/gs-ir-files/");
		Path filePath = basePath.resolve(Paths.get("converted-png", "image-1.png"));
		Path deskewedPath = Deskewer.deskewAndSave(filePath, METHOD.HOUGH_LINES);
		JsonObject jsonFields = detectFields(deskewedPath);
		System.out.println(jsonFields.encodePrettily());
		JsonObject data = processFile(basePath.relativize(deskewedPath), basePath, jsonFields);
		System.out.println(data.encodePrettily());
		Root engine = getEngine(gsPath);
		saveOcrDataInModel(engine, data);
		engine.close();

	}

	public static Root getEngine(String gsPath) {
		return new Engine(gsPath, DocClassType.class, LayoutType.class, ImgDocRel.class, DocType.class, ImgType.class, ZoneType.class, ZoneNumType.class, ConsolidatedType.class, ImgPathType.class, ImgTimestampType.class, ImgRefreshTimestampType.class,
				SupervisedType.class);
	}

	// XXX: might need to refactor the code to be able to indicate whether the file already exists (or not) or if an error occurred at some point in the method
	public static boolean registerNewFile(Root engine, Path relativeImgPath, Path basePath, Path resourcesFolder) {
		logger.info("Adding a new image ({}) ", relativeImgPath.getFileName());
		Path absolutePath = basePath.resolve(relativeImgPath);
		String filenameExt = ModelTools.generateFileName(absolutePath);
		ImgType imgType = engine.find(ImgType.class);
		try {
			ImgInstance imgInstance = imgType.addImg(filenameExt);
			engine.getCurrentCache().flush();
			if (null == imgInstance) {
				logger.error("An error has occured while saving file {}", filenameExt);
				return false;
			} else {
				imgInstance.setImgPath(relativeImgPath.toString());
				imgInstance.setImgTimestamp(ModelTools.getCurrentDate());
				engine.getCurrentCache().flush();
				try {
					Files.copy(absolutePath, resourcesFolder.resolve(filenameExt), StandardCopyOption.REPLACE_EXISTING);
				} catch (IOException e) {
					logger.error(String.format("An error has occured while copying image %s to resources folder", filenameExt), e);
				}
				return true;
			}
		} catch (RollbackException e) {
			logger.warn("The image {} has already been saved in Generic System", relativeImgPath.getFileName());
			return false;
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}

	public static JsonObject detectFields(Path imgPath) {
		try (Img deskewed = new Img(imgPath.toString())) {
			List<Rect> rects = ClassifierUsingFields.detectRects(deskewed);
			DocFields fields = DocFields.of(rects);
			JsonObject result = fields.toJsonObject();
			return result;
		} catch (Exception e) {
			throw new IllegalStateException("An error has occured while detecting the fields on file " + imgPath.toString(), e);
		}
	}

	public static JsonObject processFile(Path relativeImgPath, Path basePath, JsonObject jsonFields) {
		Path absolutePath = basePath.resolve(relativeImgPath);
		// Create a JsonObject for the answer
		JsonObject jsonObject = new JsonObject();
		jsonObject.put(FILENAME, relativeImgPath.getFileName().toString());
		jsonObject.put(ENCODED_FILENAME, ModelTools.generateFileName(absolutePath));
		jsonObject.put(DOC_TIMESTAMP, ModelTools.getCurrentDate());
		jsonObject.put(DOC_PATH, relativeImgPath.toString());

		// Get the doc fields
		DocFields fields = DocFields.of(jsonFields);

		// Get the imgFilterFunctions, and create a Map with the processed images
		Img deskewed = new Img(absolutePath.toString());
		final List<ImgFilterFunction> imgFilterFunctions = FillModelWithData.getFilterFunctions();
		Map<String, Img> imgs = new ConcurrentHashMap<>(imgFilterFunctions.size() + 1);
		imgFilterFunctions.forEach(entry -> {
			String filtername = entry.getName();
			ImgFunction function = entry.getLambda();
			logger.info("Applying algorithm {}...", filtername);
			Img img = null;

			long start = System.nanoTime();
			if ("original".equals(filtername) || "reality".equals(filtername)) {
				img = new Img(deskewed.getSrc(), true);
			} else {
				img = function.apply(deskewed);
			}
			long stop = System.nanoTime();
			logger.info("({} ms)", (stop - start) / 1_000_000);

			if (null != img) {
				imgs.put(filtername, img);
			} else {
				logger.error("An error as occured for image {} and filter {}", relativeImgPath.getFileName(), filtername);
			}
		});

		// Loop through each field, and do the OCR
		Map<String, JsonObject> result = new ConcurrentHashMap<>(fields.size() + 1);
		fields.forEach(field -> {
			logger.info("Field {}", field.getNum());
			imgs.entrySet().forEach(entry -> {
				if (!("reality".equals(entry.getKey()) || "best".equals(entry.getKey()))) {
					// Do the ocr, and store the value in the "labels" Map
					field.ocr(entry.getValue());
				}
			});
			// Loop through the "labels" Map and choose the best text
			field.consolidateOcr();

			// Store the field data in a json object
			JsonObject json = new JsonObject();
			json.put(FIELD_NUM, field.getNum());
			json.put(CONSOLIDATED, field.getConsolidated().orElse(""));
			json.put(RECT, JsonObject.mapFrom(field.getRect()));

			// Add this to the result
			result.put(field.getUid(), json);
		});
		// Store the ocr in the JsonObject
		jsonObject.put(ZONES, result);

		// Close the resources used by OpenCV
		deskewed.close();
		imgs.values().forEach(Img::close);

		// Return the result
		return jsonObject;
	}

	// Update the return type to indicate whether the document was already known, if new zones were added, etc.
	public static void saveOcrDataInModel(Root engine, JsonObject data) {
		// Parse the data
		String docPath = data.getString(DOC_PATH);
		String filenameExt = data.getString(ENCODED_FILENAME);
		Long timestamp = data.getLong(DOC_TIMESTAMP);
		JsonObject zonesResults = data.getJsonObject(ZONES);

		// Get the generics
		ImgType imgType = engine.find(ImgType.class);

		// Set the doc instance and some attributes
		ImgInstance imgInstance;
		try {
			imgInstance = imgType.addImg(filenameExt);
		} catch (RollbackException e1) {
			logger.info(String.format("File %s has already been processed by the system. Retrieving the reference...", filenameExt));
			imgInstance = imgType.getImg(filenameExt);
		}
		try {
			imgInstance.setImgPath(docPath);
			imgInstance.setImgTimestamp(timestamp);
			imgInstance.setImgRefreshTimestamp(timestamp);
		} catch (RollbackException e) {
			logger.debug("Filename or timestamp have already been set. Resuming task...");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		// Save the results for each field
		for (Entry<String, Object> entry : zonesResults) {
			logger.info("Current zone: {}", entry.getKey());
			JsonObject field = (JsonObject) entry.getValue();
			String ocr = field.getString(CONSOLIDATED);
			String rect = field.getJsonObject(RECT).encode();
			int num = field.getInteger(FIELD_NUM);
			// Try to get Zone: override if exists, otherwise create a new one
			ZoneInstance zoneInstance = imgInstance.getZone(rect);
			if (null == zoneInstance) {
				zoneInstance = imgInstance.addZone(rect);
			} else {
				logger.debug("Zone {} already known. Override consolidated ('{}' with '{}') and zone num ('{}' with '{}')", entry.getKey(), zoneInstance.getConsolidated(), ocr, zoneInstance.getZoneNum(), num);
			}
			zoneInstance.setConsolidated(ocr);
			if (zoneInstance.getSupervised() == null)
				zoneInstance.setSupervised("");
			zoneInstance.setZoneNum(num);
		}
		// Flush the cache
		engine.getCurrentCache().flush();
	}

	public static void linkImgToDocClass(Root engine, Path filePath, String defaultClassName) {
		String name = ModelTools.generateFileName(filePath);
		DocClassType docClassType = engine.find(DocClassType.class);
		ImgType imgType = engine.find(ImgType.class);

		ImgInstance imgInstance = imgType.getImg(name);

		DocClassInstance defaultDocClass = docClassType.getDocClass(defaultClassName);
		if (null == defaultDocClass) {
			logger.info("Default class {} does not exist. Creating...", defaultClassName);
			defaultDocClass = docClassType.addDocClass(defaultClassName);
		}

		ImgDocLink imgDocLink = imgInstance.getImgDocLink();
		if (null == imgDocLink) {
			DocInstance docInstance = defaultDocClass.addDocInstance(name);
			docInstance.addImgDocLink(name, imgInstance);
		} else {
			logger.info("Img {} is already in class {}", name, imgDocLink.getDocInstance().getDocClassInstance());
		}
	}

}
