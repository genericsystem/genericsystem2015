package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.classifier.Deskewer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class DeskewerVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String ACTION = "deskew";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path filePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		logger.info("Deskewing image {}", filePath);
		Path savedFile;
		synchronized (DeskewerVerticle.class) {
			savedFile = Deskewer.deskewAndSave(filePath);
		}
		if (savedFile != null)
			future.complete(savedFile.toString());
		else
			future.fail("Unable to deskew the image");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(((String) res.result()).replaceFirst(DistributedVerticle.BASE_PATH, ""), ClassifierUsingFieldsVerticle.ACTION);
		else
			throw new IllegalStateException("Error when deskewing the image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
