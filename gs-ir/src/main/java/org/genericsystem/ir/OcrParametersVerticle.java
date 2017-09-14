package org.genericsystem.ir;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrParametersVerticle computes the OCR parameters and send them to the event bus as a {@link JsonObject}.
 * 
 * @author Pierrik Lassalas
 */
public class OcrParametersVerticle extends ActionVerticle {

	public static final String ACTION = "ocr";
	private static final String RESULTS = "results";

	@Override
	public String getAction() {
		return ACTION;
	}

	private Root engine;
	private Cache cache;

	/**
	 * Default constructor. A reference to an {@link Engine} must be provided.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public OcrParametersVerticle(Root engine) {
		this.engine = engine;
		this.cache = (Cache) engine.newCache();
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME);
		JsonObject results = new JsonObject();
		cache.safeConsum(unused -> {
			JsonObject params = FillModelWithData.getOcrParameters(engine, Paths.get(imagePath));
			results.put(RESULTS, params);
		});
		if (null != results && null != results.getJsonObject(RESULTS))
			future.complete(results.getJsonObject(RESULTS));
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
