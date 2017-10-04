package org.genericsystem.ir;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class CopyToResourcesVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static final String ACTION = "copyToResources";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path filePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		try {
			Files.copy(filePath, Paths.get(DistributedVerticle.RESOURCES_FOLDER), StandardCopyOption.REPLACE_EXISTING);
			future.complete();
		} catch (IOException e) {
			future.fail(e);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			logger.info("Successfully copied {} to resources folder ({})", task.getString(DistributedVerticle.FILENAME), DistributedVerticle.RESOURCES_FOLDER);
		} else
			throw new IllegalStateException("Error while copying image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
