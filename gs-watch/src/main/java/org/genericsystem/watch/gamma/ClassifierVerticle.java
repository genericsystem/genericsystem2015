package org.genericsystem.watch.gamma;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.genericsystem.cv.Classifier;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ClassifierVerticle extends ActionVerticle {

	public ClassifierVerticle(String privateAddress, String privatePath, String ip, List<JsonObject> messages, List<JsonObject> tasks) {
		super(privateAddress, privatePath, ip, messages, tasks);
	}

	public static final String ACTION = "classification";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, String fileName, JsonObject task) {
		File file = new File(fileName);
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
	protected void handleResult(AsyncResult<Object> res, String fileName) {
		if (res.succeeded()) {
			//					long id = System.currentTimeMillis();
			//						messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, newPng).put(IP, ip).put(TYPE, CLASSIFY)));
			System.out.println("Image classified : " + res.result());
		} else {
			System.out.println("Impossible to classify image : " + fileName);
			res.cause().printStackTrace();
		}
	}
}
