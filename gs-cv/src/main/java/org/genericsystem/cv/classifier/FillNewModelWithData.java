package org.genericsystem.cv.classifier;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Consolidated;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Consolidated.ConsolidatedInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Doc;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Doc.DocInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocPath;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocPath.DocPathInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocTimestamp;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocTimestamp.DocTimestampInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgFilter;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone.ZoneInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNum;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNum.ZoneNumInstance;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.cv.comparator.ImgFilterFunction;
import org.genericsystem.cv.comparator.ImgFunction;
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

	public static final String DOC_PATH = "docPath";
	public static final String FILENAME = "filename";
	public static final String ENCODED_FILENAME = "encodedFilename";
	public static final String DOC_TIMESTAMP = "docTimestamp";
	public static final String ZONES = "zones";
	public static final String FIELD_NUM = "fieldNum";
	public static final String RECT = "rectangle";
	public static final String CONSOLIDATED = "consolidated";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv-newmodel";

	private static final String BASE_PATH = System.getenv("HOME") + "/genericsystem/gs-ir-files/";

	public static void main(String[] args) {
		Path filePath = Paths.get(BASE_PATH, "converted-png", "image-1.png");
		Path deskewedPath = Deskewer.deskewAndSave(filePath);
		JsonObject jsonFields = detectFields(deskewedPath);
		System.out.println(jsonFields.encodePrettily());
		JsonObject data = processFile(deskewedPath, jsonFields);
		System.out.println(data.encodePrettily());
		Root engine = getEngine(gsPath);
		saveOcrDataInModel(engine, data);
		engine.close();

	}

	public static Root getEngine(String gsPath) {
		return new Engine(gsPath, ImgFilter.class, ImgFilterInstance.class, Doc.class, DocInstance.class, Zone.class, ZoneInstance.class, ZoneNum.class, ZoneNumInstance.class, Consolidated.class, ConsolidatedInstance.class, DocPath.class,
				DocPathInstance.class, DocTimestamp.class, DocTimestampInstance.class);
	}

	public static boolean registerNewFile(Root engine, Path imgPath, Path resourcesFolder) {
		logger.info("Adding a new image ({}) ", imgPath.getFileName());
		String filenameExt = ModelTools.generateFileName(imgPath);
		Doc doc = engine.find(Doc.class);
		DocInstance docInstance = doc.setDoc(filenameExt);
		engine.getCurrentCache().flush();
		if (null == docInstance) {
			logger.error("An error has occured while saving file {}", filenameExt);
			return false;
		} else {
			Path relative = Paths.get(BASE_PATH).relativize(imgPath);
			docInstance.setDocPath(relative.toString());
			docInstance.setDocTimestamp(ModelTools.getCurrentDate());
			engine.getCurrentCache().flush();
			try {
				Files.copy(imgPath, resourcesFolder.resolve(filenameExt), StandardCopyOption.REPLACE_EXISTING);
			} catch (IOException e) {
				logger.error(String.format("An error has occured while copying image %s to resources folder", filenameExt), e);
			}
			return true;
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

	public static JsonObject processFile(Path imgPath, JsonObject jsonFields) {
		if (!imgPath.isAbsolute())
			throw new IllegalArgumentException("The provided path must be absolute. Got instead: " + imgPath.toString());
		// Create a JsonObject for the answer
		JsonObject jsonObject = new JsonObject();
		jsonObject.put(FILENAME, imgPath.getFileName().toString());
		jsonObject.put(ENCODED_FILENAME, ModelTools.generateFileName(imgPath));
		jsonObject.put(DOC_TIMESTAMP, ModelTools.getCurrentDate());
		try {
			// Case where the given Path is absolute
			jsonObject.put(DOC_PATH, Paths.get(BASE_PATH).relativize(imgPath).toString());
		} catch (IllegalArgumentException e) {
			logger.debug("Unable to find a common path between BASE_PATH and imgPath. Using the provided path instead.", e);
			jsonObject.put(DOC_PATH, imgPath.toString());
		}

		// Get the doc fields
		DocFields fields = DocFields.of(jsonFields);

		// Get the imgFilterFunctions, and create a Map with the processed images
		Img deskewed = new Img(imgPath.toString());
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
				logger.error("An error as occured for image {} and filter {}", imgPath.getFileName(), filtername);
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

	public static void saveOcrDataInModel(Root engine, JsonObject data) {
		// Parse the data
		String docPath = data.getString(DOC_PATH);
		String filenameExt = data.getString(ENCODED_FILENAME);
		Long timestamp = data.getLong(DOC_TIMESTAMP);
		JsonObject zonesResults = data.getJsonObject(ZONES);

		// Get the generics
		Doc doc = engine.find(Doc.class);

		// Set the doc instance and some attributes
		DocInstance docInstance = doc.setDoc(filenameExt);
		try {
			docInstance.setDocPath(docPath);
			docInstance.setDocTimestamp(timestamp);
		} catch (RollbackException e) {
			logger.debug("Filename or timestamp have already been set. Resuming task...");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		// Save the results for each field
		zonesResults.forEach(entry -> {
			logger.info("Current zone: {}", entry.getKey());
			JsonObject field = (JsonObject) entry.getValue();
			String ocr = field.getString(CONSOLIDATED);
			JsonObject rect = field.getJsonObject(RECT);
			ZoneInstance zoneInstance = docInstance.setZone(rect.encode());
			zoneInstance.setConsolidated(ocr);
			zoneInstance.setZoneNum(field.getInteger(FIELD_NUM));
		});

		// Flush the cache
		engine.getCurrentCache().flush();
	}

}
