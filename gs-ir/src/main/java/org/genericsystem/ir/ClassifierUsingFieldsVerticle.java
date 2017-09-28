package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.classifier.ClassifierUsingFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ClassifierUsingFieldsVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String ERROR_MSG = "Failed to classify image";

	public static final String ACTION = "classifyUsingFields";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path filePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		logger.info("Classifying image {}", filePath);
		Path savedFile = ClassifierUsingFields.classify(filePath);
		if (savedFile != null)
			future.complete(savedFile.toString());
		else
			future.complete(ERROR_MSG);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			String message = (String) res.result();
			if (message == ERROR_MSG)
				addTask(message.replaceFirst(DistributedVerticle.BASE_PATH, ""), NewClassCreatorVerticle.ACTION);
			// else
			// addTask(message.replaceFirst(DistributedVerticle.BASE_PATH, ""), AddImageToEngineVerticle.ACTION);
		} else
			throw new IllegalStateException("Error when deskewing the image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
