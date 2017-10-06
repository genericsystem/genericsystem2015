package org.genericsystem.cv.utils;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.Path;

import org.genericsystem.cv.Img;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Create a new class when a document was not classified into an existing one.
 * 
 * @author Fabienne Ducroquet
 * @author Pierrik Lassalas
 */
public class NewClassCreator {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static Path createNewClass(Path classesDirectory, Path imgPath) {
		Path newClassDir = null;
		synchronized (NewClassCreator.class) {
			newClassDir = classesDirectory.resolve(String.valueOf(System.nanoTime()));
			newClassDir.toFile().mkdirs();
		}
		Path savedFile = newClassDir.resolve(imgPath.getFileName());
		try (Img img = new Img(imgPath.toString())) {
			synchronized (NewClassCreator.class) {
				if (savedFile.toFile().exists()) {
					String[] fileNameParts = imgPath.getFileName().toString().split("\\.(?=[^\\.]+$)");
					savedFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], newClassDir.toFile()).toPath();
				}
			}
			Imgcodecs.imwrite(savedFile.toString(), img.getSrc());
			logger.debug("Saved file {} in class {}", imgPath.getFileName(), newClassDir);
			return savedFile;
		} catch (IOException e) {
			logger.error("Error while saving image {} in class {}.", e, imgPath.getFileName(), newClassDir);
			return null;
		}
	}
}
