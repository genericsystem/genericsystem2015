package org.genericsystem.ir;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.classifier.FillNewModelWithData;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrWorkerVerticle computes the OCR for a given file, and send the results as a {@link JsonObject} to the event bus.
 * 
 * @author Pierrik Lassalas
 */
public class OcrWorkerVerticle extends ActionVerticle {

	public static final String ACTION = "ocrWorker";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		JsonObject fields = task.getJsonObject(DistributedVerticle.JSON_OBJECT);
		Path imgPath = Paths.get(DistributedVerticle.BASE_PATH).resolve(Paths.get(task.getString(DistributedVerticle.FILENAME)));
		JsonObject ocrData = FillNewModelWithData.processFile(imgPath, fields);
		if (null != ocrData)
			future.complete(ocrData);
		else
			future.fail("Unable to do the OCR");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), (JsonObject) res.result(), OcrPersistenceVerticle.ACTION);
		else
			throw new IllegalStateException("Error when processing the image " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}

}
