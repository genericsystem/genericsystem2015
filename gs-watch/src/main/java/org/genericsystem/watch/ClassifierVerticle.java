package org.genericsystem.watch;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.Classifier.CompareFeatureResult;
import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

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
			// Only one access to classesDirectory at a time to avoid duplicate classes.
			Path classesDirectory = Paths.get("..", "gs-cv", "classes");
			classesDirectory.toFile().mkdirs();
			synchronized (ClassifierVerticle.class) {
				CompareFeatureResult bestClass = Classifier.selectBestClass(classesDirectory, img);
				System.gc();
				System.runFinalization();
				Path matchingClassDir;
				Mat alignedImage = null;
				if (bestClass != null) {
					matchingClassDir = Paths.get("..", ("gs-cv/" + bestClass.getImgClass().getDirectory()).split(File.separator));
					alignedImage = bestClass.getImg();
				} else {
					matchingClassDir = classesDirectory.resolve(System.nanoTime() + "");
					matchingClassDir.toFile().mkdirs();
					try {
						alignedImage = new Img(img).cropAndDeskew().getSrc();
					} catch (Exception e) {
						matchingClassDir.toFile().delete();
						log.warn("Error while deskewing new image " + newFile.toString() + " to create new class, new class not created.", e);
						future.fail("Impossible to create new class.");
						// TODO: Store the image somewhere else.
					}
				}
				String[] fileNameParts = newFile.getFileName().toString().split("\\.(?=[^\\.]+$)");
				File savedFile;
				try {
					savedFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], matchingClassDir.toFile());
					Imgcodecs.imwrite(savedFile.toString(), alignedImage);
					JsonObject watchMsg = new JsonObject().put("filename", savedFile.toString());
					vertx.eventBus().publish(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS, watchMsg.encodePrettily());
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
