package org.genericsystem.watch;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class ClassifierVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new ClassifierVerticle());
	}

	@Override
	public void start() {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.PNG_WATCHER_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			Path newFile = Paths.get(".", message.body().split(File.separator));
			Mat img = Imgcodecs.imread(newFile.toString());
			Path matchingClassDir = null;
			Mat alignedImage = null;
			Path classesDirectory = Paths.get("..", "gs-cv", "classes");
			classesDirectory.toFile().mkdirs();
			// Only one access to classesDirectory at a time to avoid duplicate classes.
			synchronized (ClassifierVerticle.class) {
				try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(classesDirectory, Files::isDirectory)) {
					for (Path path : directoryStream) {
						ImgClass imgClass = new ImgClass(null, path.toString());
						Mat alignedImage_ = Classifier.compareFeature(img, imgClass.getMean().getSrc(), Classifier.MATCHING_THRESHOLD);
						if (alignedImage_ != null) {
							if (matchingClassDir != null)
								log.warn("Two matching classes found for file " + newFile + " : " + matchingClassDir + " and " + path);
							matchingClassDir = path;
							alignedImage = alignedImage_;
						}
						System.gc();
						System.runFinalization();
					}
				} catch (IOException e) {
					log.error("IOException:", e);
				}
				if (matchingClassDir == null) {
					matchingClassDir = classesDirectory.resolve(System.nanoTime() + "");
					matchingClassDir.toFile().mkdirs();
					try {
						alignedImage = new Img(img).cropAndDeskew().getSrc();
					} catch (Exception e) {
						matchingClassDir.toFile().delete();
						log.warn("Error while deskewing new image " + newFile.toString() + " to create new class, new class not created.", e);
						// TODO: Store the image somewhere else.
					}
				}
				String[] fileNameParts = newFile.getFileName().toString().split("\\.(?=[^\\.]+$)");
				File savedFile;
				try {
					savedFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], matchingClassDir.toFile());
					Imgcodecs.imwrite(savedFile.toString(), alignedImage);
				} catch (IOException e) {
					log.warn("IOException: ", e);
				}
			}
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
