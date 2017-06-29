package org.genericsystem.cv.comparator;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;



/**
 * The GetDataInModel class can analyze a batch of images and store the OCR text
 * for each zone and each document in GS.
 * 
 * @author Pierrik Lassalas
 *
 */
public class GetDataInModel {

	private static Logger log = LoggerFactory.getLogger(GetDataInModel.class);
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model2/";
	private final static Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class,
			ZoneText.class);

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] mainArgs) {
		engine.newCache().start();
		compute();
		engine.close();
	}

	public static void compute() {
		final String docType = "id-fr-front";
		final String imgClassDirectory = "classes/" + docType;
		final String imgDirectory = imgClassDirectory + "/ref2/";

		log.info("imgClassDirectory = {} ", imgClassDirectory);

		// Get the necessary classes from the engine
		DocClass docClass = engine.find(DocClass.class);
		Generic doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		// Save the current document class
		DocClassInstance docClassInstance = docClass.addDocClass(docType);

		// Set all the filter names
		Map<String,Function<Img,Img>> imgFilters = fillAlgorithmMap();

		// Load the accurate zones
		final Zones zones = Zones.load(imgClassDirectory);

		// Save the zones
		zones.getZones().forEach(z -> {
			log.info("Adding zone n° {}", z.getNum());
			docClassInstance.addZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width, z.getRect().height);
		});

		// Save the filternames
		imgFilters.entrySet().forEach(entry -> {
			log.info("Adding algorithm : {} ", entry.getKey());
			imgFilter.addImgFilter(entry.getKey());
		});

		// Persist the changes
		engine.getCurrentCache().flush();

		// Process each file in folder imgDirectory
		Arrays.asList(new File(imgDirectory).listFiles((dir, name) -> name.endsWith(".png"))).stream().forEach(file -> {
			engine.getCurrentCache().mount();
			log.info("\nProcessing file: {}", file.getName());
			// Draw the image's zones + numbers
			Img originalImg = new Img(Imgcodecs.imread(file.getPath()));
			Img annotedImg = new Img(originalImg.getSrc());
			zones.draw(annotedImg, new Scalar(0, 255, 0), 3);
			zones.writeNum(annotedImg, new Scalar(0, 0, 255), 3);
			// Copy the images to the resources folder
			// TODO implement a filter mechanism to avoid creating
			// duplicates in a public folder
			Imgcodecs.imwrite(System.getProperty("user.dir") + "/src/main/resources/" + file.getName(),
					annotedImg.getSrc());
			// Save the current file (document)
			DocInstance docInstance = docClassInstance.addDoc(docClassInstance, doc, file.getName());

			// Create a map of treated Img
			log.info("Converting image {}:", file.getName());
			Map<String, Img> imgs = new HashMap<>();
			imgFilters.entrySet().forEach(entry -> {
				log.info("Applying algorithm {}...", entry.getKey());
				Img img = null;
				if ("original".equals(entry.getKey()) || "reality".equals(entry.getKey())) {
					img = originalImg.bgr2Gray();
					imgs.put(entry.getKey(), img);
				} else {
					img = entry.getValue().apply(originalImg);
				}
			});

			// Process each zone
			zones.getZones().stream().forEach(z -> {
				log.info("Zone n° {}", z.getNum());
				// Save the zone
				ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
				imgs.entrySet().forEach(entry -> {
					if ("reality".equals(entry.getKey()) && null != zoneText.getZoneText(docInstance, zoneInstance,
							imgFilter.getImgFilter(entry.getKey()))) {
						// Do not proceed to OCR if the real values are already stored in GS
					} else {
						// Get the OCR text
						String ocrText = z.ocr(entry.getValue());
						log.info("filter {} => {}", entry.getKey(), ocrText.trim());
						// Add the text to the corresponding zone
						zoneText.addZoneText(ocrText, docInstance, zoneInstance, imgFilter.getImgFilter(entry.getKey()));
					}
				});
				// Call the garbage collector to free the resources used by
				// OpenCV
				System.gc();
			});
			engine.getCurrentCache().flush();
			engine.getCurrentCache().unmount();
		});
		engine.getCurrentCache().flush();
	}

	private static Map<String, Function<Img, Img>> fillAlgorithmMap(){
		final Map<String,Function<Img,Img>> map = new HashMap<>();
		map.put("original", Img::bgr2Gray);
		map.put("reality", Img::bgr2Gray);
		map.put("bernsen", Img::bernsen);
		map.put("equalizeHisto", Img::equalizeHisto);
		map.put("equalizeHistoAdaptative", Img::equalizeHistoAdaptative);
		return map;
	}
}
