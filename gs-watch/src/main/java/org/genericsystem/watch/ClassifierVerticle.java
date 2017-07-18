package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.opencv.core.Core;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class ClassifierVerticle extends AbstractVerticle {

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
			Path newFile = Paths.get(message.body());
			System.out.println(">>> New file to classify: " + newFile);
			// Only one access to classesDirectory at a time to avoid duplicate classes.
			Path classesDirectory = Paths.get("..", "gs-cv", "classes");
			classesDirectory.toFile().mkdirs();
			Path savedFile;
			synchronized (ClassifierVerticle.class) {
				savedFile = Classifier.classify(classesDirectory, newFile);
			}
			if (savedFile != null) {
				vertx.eventBus().publish(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS, savedFile.toString());
				future.complete();
			} else
				future.fail("Impossible to classify image " + newFile);
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
