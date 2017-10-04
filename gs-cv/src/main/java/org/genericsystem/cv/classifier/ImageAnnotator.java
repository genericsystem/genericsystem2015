package org.genericsystem.cv.classifier;

import java.nio.file.Path;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.model.ModelTools;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;

import io.vertx.core.json.JsonObject;

public class ImageAnnotator {

	public static Path annotateImage(Path imgPath, Path resourcesFolder, JsonObject fields) {
		String filename = ModelTools.generateFileName(imgPath);
		Path savedFile = resourcesFolder.resolve(filename);
		DocFields docFields = DocFields.of(fields);

		try (Img src = new Img(imgPath.toString()); Img annotated = docFields.annotateImage(src, 2d, new Scalar(0, 0, 255), 2)) {
			Imgcodecs.imwrite(savedFile.toString(), annotated.getSrc());
			return savedFile;
		} catch (Exception e) {
			throw new IllegalStateException("An error has occured while saving file " + savedFile + " to resources folder", e);
		}
	}

}
