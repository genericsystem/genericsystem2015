package org.genericsystem.watch.gamma;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ClassifierVerticle extends ActionVerticle {

	public ClassifierVerticle(String privateAddress, String privatePath, String ip) {
		super(privateAddress, privatePath, ip);
	}

	public static final String ACTION = "classification";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		File file = new File(task.getString(DistributedVerticle.FILENAME));
		Path savedFile;
		synchronized (ClassifierVerticle.class) {
			savedFile = Classifier.classify(Paths.get("../gs-cv/classes/"), file.toPath());
		}
		if (savedFile != null)
			future.complete(savedFile);
		else
			future.fail("Impossible to classify image.");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			//					long id = System.currentTimeMillis();
			//						messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, newPng).put(IP, ip).put(TYPE, CLASSIFY)));
			System.out.println("Image classified : " + res.result());
		} else {
			System.out.println("Impossible to classify image : " + task.getString(DistributedVerticle.FILENAME));
			res.cause().printStackTrace();
		}
	}
}
