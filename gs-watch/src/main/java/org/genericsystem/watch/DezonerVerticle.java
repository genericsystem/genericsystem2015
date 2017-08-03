package org.genericsystem.watch;

import java.lang.invoke.MethodHandles;

import org.genericsystem.cv.Zones;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The DezonerVerticle receives a message from the event bus when a new image has been added to a class. If this class has already been dezoned, a message will be sent to the {@link OcrVerticle}.
 * 
 * @author Pierrik Lassalas
 */
public class DezonerVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String ACTION = "dezoner";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = task.getString(DistributedVerticle.FILENAME);
		if (Zones.isZonesFilePresent(imagePath)) {
			// The zones file was found, proceed trough OCR directly
			future.complete(OcrVerticle.ACTION);
		} else {
			// No zones file was found, need to define the zones manually
			// TODO: replace the future.fail by a notification to the system that a zone needs to be defined for this file
			future.fail("No accurate zones found for " + imagePath);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), (String) res.result());
		else
			logger.info("No zones defined for file {}.", task.getString(DistributedVerticle.FILENAME));
	}
}
