package org.genericsystem.watch;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.ImgClass;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;

public class ClassifierVerticle extends FileCreateEventsHandlerVerticle {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public ClassifierVerticle() {
		super(VerticleDeployer.PNG_WATCHER_ADDRESS);
	}

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new ClassifierVerticle());
	}

	@Override
	public void handle(Path newFile) {
		Mat img = Imgcodecs.imread(newFile.toString(), Imgcodecs.CV_LOAD_IMAGE_COLOR);
		ImgClass matchingClass = null;
		Mat alignedImage = null;
		Path classesDirectory = Paths.get("..", "gs-cv", "classes");
		classesDirectory.toFile().mkdirs();
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(classesDirectory, Files::isDirectory)) {
			for (Path path : directoryStream) {
				ImgClass imgClass = new ImgClass(null, path.toString());
				Mat alignedImage_ = Classifier.compareFeature(img, imgClass.getMean().getSrc(), Classifier.MATCHING_THRESHOLD);
				if (alignedImage_ != null) {
					if (matchingClass != null)
						throw new IllegalStateException("Two matching classes found");
					matchingClass = imgClass;
					alignedImage = alignedImage_;
				}
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
		if (matchingClass != null)
			Imgcodecs.imwrite(matchingClass.getDirectory() + "/" + newFile.getFileName().toString(), alignedImage);
		else {
			Path unclassifiedDir = Paths.get("..", "gs-cv", "unclassified");
			unclassifiedDir.toFile().mkdirs();
			Imgcodecs.imwrite(unclassifiedDir.toString() + "/" + newFile.getFileName().toString(), img);
		}
	}
}
