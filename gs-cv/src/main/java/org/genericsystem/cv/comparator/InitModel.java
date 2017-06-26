package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.comparator.InitModel.InitScript;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunScript(InitScript.class)
@DependsOnModel({ Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class })
public class InitModel extends RootTagImpl {

	private static Logger log = LoggerFactory.getLogger(InitModel.class);

	public static void main(String[] mainArgs) {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		ApplicationServer.startSimpleGenericApp(mainArgs, InitModel.class, "/gs-cv_model");
	}

	public static class InitScript implements Script {

		@Override
		public void run(Root engine) {
			String docType = "id-fr-front";
			String imgClassDirectory = "classes/" + docType;
			log.info("imgClassDirectory = {} ", imgClassDirectory);

			// Get the necessary classes from the engine
			DocClass docClass = engine.find(DocClass.class);
			Generic doc = engine.find(Doc.class);
			ZoneText zoneText = engine.find(ZoneText.class);
			ImgFilter imgFilter = engine.find(ImgFilter.class);

			// Save the current document class
			DocClassInstance docClassInstance = docClass.addDocClass(docType);

			// Get all the filternames
			String[] imgF = { "reality", "original", "abutaleb", "bernsen", "brink", "djvu", "niblack", "otsu",
					"sauvola", "shading-subtraction", "tsai", "white-rohrer" };
			List<String> imgFilters = Arrays.asList(imgF);

			// Load the accurate zones
			Zones zones = Zones.load(imgClassDirectory);

			// Save the zones
			zones.getZones().forEach(z -> {
				log.info("Adding zone n° {}", z.getNum());
				docClassInstance.addZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width,
						z.getRect().height);
			});

			// Save the filternames
			imgFilters.forEach(f -> {
				log.info("Adding filter : {} ", f);
				imgFilter.addImgFilter(f);
			});

			// Persist the changes
			engine.getCurrentCache().flush();

			// Process each file in the subfolder "/ref"
			Arrays.asList(new File(imgClassDirectory + "/ref/").listFiles((dir, name) -> name.endsWith(".png")))
					.stream().forEach(file -> {
						log.info("\nProcessing file: {}", file.getName());
						// Draw the image's zones + numbers
						Img originalImg = new Img(Imgcodecs.imread(file.getPath()));
						zones.draw(originalImg, new Scalar(0, 255, 0), 3);
						zones.writeNum(originalImg, new Scalar(0, 0, 255), 3);
						// Copy the images to the resources folder
						// TODO implement a filter mechanism to avoid creating
						// duplicates in a public folder
						Imgcodecs.imwrite(System.getProperty("user.dir") + "/src/main/resources/" + file.getName(),
								originalImg.getSrc());
						// Save the file
						// DocInstance docInstance = (DocInstance)
						// docClassInstance.setHolder(doc, file.getName());
						DocInstance docInstance = docClassInstance.addDoc(docClassInstance, doc, file.getName());

						// Process each zone
						zones.getZones().stream().forEach(z -> {
							log.info("Zone n° {}", z.getNum());
							// Save the zone
							ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
							for (String filter : imgFilters) {
								Img filteredImage;
								// "reality" is initialized with the original picture if the text has not been filled yet
								if ("original".equals(filter) || "reality".equals(filter)) {
									if ("reality".equals(filter) && null != zoneText.getZoneText(docInstance,
											zoneInstance, imgFilter.getImgFilter(filter))) {
										continue;
									}
									filteredImage = new Img(
											Imgcodecs.imread(imgClassDirectory + "/ref/" + file.getName()));
								} else {
									filteredImage = new Img(Imgcodecs.imread(imgClassDirectory + "/mask/" + filter + "/"
											+ file.getName().replace(".png", "") + "-" + filter + ".png"));
								}
								// Get the OCR text
								String ocrText = z.ocr(filteredImage);
								log.info("filter {} => {}", filter, ocrText.trim());
								// Add the text to the corresponding zone
								zoneText.addZoneText(ocrText, docInstance, zoneInstance,
										imgFilter.getImgFilter(filter));
							}
						});
						// Call the garbage collector to free the resources
						System.gc();
					});

			engine.getCurrentCache().flush();
		}
	}
}
