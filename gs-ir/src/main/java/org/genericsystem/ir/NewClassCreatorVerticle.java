package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.utils.NewClassCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class NewClassCreatorVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static final String ACTION = "newClassCreation";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path filePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		logger.info("Creating a new class for image ", filePath);
		Path savedFile = NewClassCreator.createNewClass(Paths.get(DistributedVerticle.BASE_PATH, "/classes/"), filePath);
		if (savedFile != null)
			future.complete(savedFile.toString());
		else
			future.fail("Unable to create the class");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			Path path = Paths.get((String) res.result());
			Path relative = Paths.get(DistributedVerticle.BASE_PATH).relativize(path);
			logger.info("Created new class {} for file {}", relative.getParent(), path.getFileName());
			addTask(relative.toString(), AddImageToEngineVerticle.ACTION);
		} else
			throw new IllegalStateException("Error while creating a new class for image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
