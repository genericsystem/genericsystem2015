package org.genericsystem.cv;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.InitModel.InitScript;
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

@RunScript(InitScript.class)
@DependsOnModel({ Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class })
public class InitModel extends RootTagImpl {

	public static void main(String[] mainArgs) {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		ApplicationServer.startSimpleGenericApp(mainArgs, InitModel.class, "/gs-cv_model");
	}

	public static class InitScript implements Script {

		@Override
		public void run(Root engine) {

			String docType = "id-fr-front";

			String imgClassDirectory = "classes/" + docType;

			DocClass docClass = engine.find(DocClass.class);
			DocClassInstance docClassInstance = docClass.addDocClass(docType);

			Generic doc = engine.find(Doc.class);

			ZoneText zoneText = engine.find(ZoneText.class);
			ImgFilter imgFilter = engine.find(ImgFilter.class);

			String[] imgF = { "reality", "original", "abutaleb", "bernsen", "brink", "djvu", "niblack", "otsu",
					"sauvola", "shading-subtraction", "tsai", "white-rohrer" };
			List<String> imgFilters = Arrays.asList(imgF);

			Zones zones = Zones.load(imgClassDirectory);
			for (Zone z : zones.getZones()) {
				System.out.println("zone n°" + z.getNum());
				docClassInstance.addZone(z.getNum(), z.getRect().x, z.getRect().y, z.getRect().width,
						z.getRect().height);
			}

			for (String f : imgFilters) {
				System.out.println("filter : " + f);
				imgFilter.addImgFilter(f);
			}
			engine.getCurrentCache().flush();
			for (File file : new File(imgClassDirectory + "/ref/").listFiles((dir, name) -> name.endsWith(".png"))) {
				System.out.println(file.getName());
				// draw and save the document with zones
				Img originalImg = new Img(Imgcodecs.imread(file.getPath()));
				zones.draw(originalImg, new Scalar(0, 255, 0), 3);
				zones.writeNum(originalImg, new Scalar(0, 0, 255), 3);
				Imgcodecs.imwrite(System.getProperty("user.dir") + "/src/main/resources/" + file.getName(),
						originalImg.getSrc());
				DocInstance docInstance = (DocInstance) docClassInstance.setHolder(doc, file.getName());
				for (Zone z : zones.getZones()) {
					System.out.println("zone n°" + z.getNum());
					ZoneInstance zoneInstance = docClassInstance.getZone(z.getNum());
					for (String filter : imgFilters) {
						Img filteredImage;
						// reality is initialized with the original picture
						// and will be manually corrected then
						if ("original".equals(filter) || "reality".equals(filter))
							filteredImage = new Img(Imgcodecs.imread(imgClassDirectory + "/ref/" + file.getName()));
						else
							filteredImage = new Img(Imgcodecs.imread(imgClassDirectory + "/mask/" + filter + "/"
									+ file.getName().replace(".png", "") + "-" + filter + ".png"));
						String s = z.ocr(filteredImage);
						System.out.println("filter " + filter + " => " + s);
						zoneText.addZoneText(s, docInstance, zoneInstance, imgFilter.getImgFilter(filter));
					}
				}
			}
			engine.getCurrentCache().flush();
		}
	}
}
