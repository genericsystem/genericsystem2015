package org.genericsystem.watch;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.opencv.core.Core;

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
			// Only one access to classesDirectory at a time to avoid duplicate classes.
			Path classesDirectory = Paths.get("..", "gs-cv", "classes");
			classesDirectory.toFile().mkdirs();
			File savedFile;
			synchronized (ClassifierVerticle.class) {
				savedFile = Classifier.classify(classesDirectory, newFile);
			}
			if (savedFile != null) {
				JsonObject watchMsg = new JsonObject().put("filename", savedFile.toString());
				vertx.eventBus().publish(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS, watchMsg.encodePrettily());
				future.complete();
			} else
				future.fail("Impossible to classify image " + newFile);
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
