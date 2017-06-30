package org.genericsystem.cv.comparator;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Stream;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The FillModelWithData class can analyze a batch of images and store the OCR
 * text for each zone and each document in GS.
 * 
 * @author Pierrik Lassalas
 *
 */
public class FillModelWithData {

	private static Logger log = LoggerFactory.getLogger(FillModelWithData.class);
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model2/";
	private final static Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class,
			ZoneText.class, Score.class, MeanLevenshtein.class);

	private static final String docType = "id-fr-front";

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		System.out.println("OpenCV core library laoded");
	}

	public static void main(String[] mainArgs) {
		engine.newCache().start();
		compute();
		// cleanModel();
		engine.close();
		// doImgOcr(Paths.get("/home/middleware/git/genericsystem2015/gs-cv/classes/id-fr-front"));
	}

	public static Map<String, Function<Img, Img>> getFiltersMap() {
		final Map<String, Function<Img, Img>> map = new HashMap<>();
		map.put("original", Img::bgr2Gray);
		map.put("reality", Img::bgr2Gray);
		map.put("bernsen", Img::bernsen);
		map.put("equalizeHisto", Img::equalizeHisto);
		map.put("equalizeHistoAdaptative", Img::equalizeHistoAdaptative);
		return map;
	}

	public static void doImgOcr(Path imagePath) {
		final Path imgClassDirectory = imagePath.getParent();
		final String docType = imgClassDirectory.getName(imgClassDirectory.getNameCount() - 1).toString();
		final String imgDirectory = imgClassDirectory.toString() + "/ref2/";
		log.info("imgDirectory = {} ", imgDirectory);

		engine.newCache().start();

		DocClass docClass = engine.find(DocClass.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		DocClassInstance docClassInstance = docClass.addDocClass(docType);

		final Map<String, Function<Img, Img>> imgFilters = getFiltersMap();
		final Zones zones = Zones.load(imgClassDirectory.toString());

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
		engine.getCurrentCache().flush();
		
		File file = new File(imagePath.toString());
		processFile(file, docClassInstance, zones, imgFilters.entrySet().stream());
		engine.getCurrentCache().flush();
		
		engine.close();
	}
	
	private void initComputation(){
		// TODO: refactor the code in common
	}

	private static void compute() {
		final String imgClassDirectory = "classes/" + docType;
		final String imgDirectory = imgClassDirectory + "/ref2/";

		log.info("imgClassDirectory = {} ", imgClassDirectory);

		// Get the necessary classes from the engine
		DocClass docClass = engine.find(DocClass.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		// Save the current document class
		DocClassInstance docClassInstance = docClass.addDocClass(docType);

		// Set all the filter names
		Map<String, Function<Img, Img>> imgFilters = filterOptimizationMap();

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
		Arrays.asList(new File(imgDirectory).listFiles((dir, name) -> name.endsWith(".png"))).forEach(file -> {
			// engine.getCurrentCache().mount();
			processFile(file, docClassInstance, zones, imgFilters.entrySet().stream());
			// engine.getCurrentCache().flush();
			// engine.getCurrentCache().unmount();
			engine.getCurrentCache().flush();
		});
		engine.getCurrentCache().flush();
	}

	private static void processFile(File file, DocClassInstance docClassInstance, Zones zones,
			Stream<Entry<String, Function<Img, Img>>> imgFilters) {
		log.info("\nProcessing file: {}", file.getName());

		Generic doc = engine.find(Doc.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		// Save the current file (document)
		DocInstance docInstance = docClassInstance.addDoc(doc, file.getName());
		engine.getCurrentCache().flush();

		// Create a map of Imgs
		Img originalImg = new Img(Imgcodecs.imread(file.getPath()));
		Map<String, Img> imgs = new HashMap<>();
		imgFilters.forEach(entry -> {
			log.info("Applying algorithm {}...", entry.getKey());
			Img img = null;
			if ("original".equals(entry.getKey()) || "reality".equals(entry.getKey()))
				img = originalImg.bgr2Gray();
			else
				img = entry.getValue().apply(originalImg);
			if (null != img)
				imgs.put(entry.getKey(), img);
			else
				log.error("An error as occured for image {} and filter {}", file.getName(), entry.getKey());
		});

		// Process each zone
		zones.getZones().forEach(z -> {
			log.info("Zone n° {}", z.getNum());
			ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
			imgs.entrySet().forEach(entry -> {
				if ("reality".equals(entry.getKey()) && null != zoneText.getZoneText(docInstance, zoneInstance,
						imgFilter.getImgFilter(entry.getKey()))) {
					// Do not proceed to OCR if the real values are known
				} else {
					String ocrText = z.ocr(entry.getValue());
					zoneText.addZoneText(ocrText.trim(), docInstance, zoneInstance,
							imgFilter.getImgFilter(entry.getKey()));
				}
			});
			engine.getCurrentCache().flush();
			// Call the garbage collector to free the resources used by
			// OpenCV
			System.gc();
		});
	}

	private static Map<String, Function<Img, Img>> filterOptimizationMap() {
		final Map<String, Function<Img, Img>> imgFilters = new HashMap<>();
		List<Integer> blockSizes = Arrays.asList(new Integer[] { 7, 9, 11, 15, 17, 21, 27, 37 });
		List<Double> ks = Arrays.asList(new Double[] { -1.0, -0.8, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1 });
		for (Integer bs : blockSizes) {
			for (Double k : ks) {
				imgFilters.put("niblack" + "_" + bs + "_" + k.toString().replace("-", "m"),
						img -> img.niblackThreshold(bs, k));
			}
		}
		return imgFilters;
	}

	@SuppressWarnings({ "unused", "unchecked", "rawtypes" })
	private static void cleanModel() {
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
		});

		System.out.println("Done!");
	}
}
