package org.genericsystem.ir;

import org.genericsystem.common.Root;
import org.genericsystem.cv.newmodel.FillNewModelWithData;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrPersistenceVerticle persists the informations from the OCR into Generic System.
 */
public class OcrPersistenceVerticle extends ActionPersistentVerticle {

	public static final String ACTION = "ocrPersist";

	public OcrPersistenceVerticle(Root engine) {
		super(engine);
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		JsonObject data = task.getJsonObject(DistributedVerticle.JSON_OBJECT);
		try {
			FillNewModelWithData.saveOcrDataInModel(engine, data);
			future.complete();
		} catch (RuntimeException e) {
			future.fail(e);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.failed())
			throw new IllegalStateException("Exception in OcrPersistenceVerticle.", res.cause());
		else
			addTask(task.getString(DistributedVerticle.FILENAME), LinkImgToDocClassVerticle.ACTION);
	}

}
