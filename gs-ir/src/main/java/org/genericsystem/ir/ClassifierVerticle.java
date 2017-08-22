package org.genericsystem.ir;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ClassifierVerticle extends ActionVerticle {

	public static final String ACTION = "classification";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		File file = new File(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		Path savedFile;
		synchronized (ClassifierVerticle.class) {
			savedFile = Classifier.classify(Paths.get("../gs-cv/classes/"), file.toPath());
		}
		if (savedFile != null)
			future.complete(savedFile.toString());
		else
			future.fail("Impossible to classify image.");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask((String) res.result(), AddImageToEngineVerticle.ACTION);
		else
			throw new IllegalStateException("Error when classifying the image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
