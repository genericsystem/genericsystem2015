package org.genericsystem.ir;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrParametersVerticle computes the OCR parameters and send them to the event bus as a {@link JsonObject}.
 * 
 * @author Pierrik Lassalas
 */
public class OcrParametersVerticle extends ActionPersistentVerticle {

	public static final String ACTION = "ocr";

	@Override
	public String getAction() {
		return ACTION;
	}

	public OcrParametersVerticle(Root engine) {
		super(engine);
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME);
		JsonObject params = FillModelWithData.buildOcrParameters(engine, Paths.get(imagePath));
		if (null != params)
			future.complete(params);
		else
			future.fail("Unable to get the OCR parameters");
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			if (!((JsonObject) res.result()).isEmpty())
				addTask(task.getString(DistributedVerticle.FILENAME), (JsonObject) res.result(), OcrWorkerVerticle.ACTION);
		} else
			throw new IllegalStateException("Exception in OcrParametersVerticle.", res.cause());
	}
}
