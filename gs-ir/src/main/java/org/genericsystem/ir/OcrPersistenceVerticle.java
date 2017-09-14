package org.genericsystem.ir;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrPersistenceVerticle persists the informations from the OCR into Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class OcrPersistenceVerticle extends ActionVerticle {

	public static final String ACTION = "ocrPersist";

	private Root engine;
	private Cache cache;

	/**
	 * Default constructor. A reference to an {@link Engine} must be provided to be able to save the results.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public OcrPersistenceVerticle(Root engine) {
		this.engine = engine;
		this.cache = (Cache) engine.newCache();
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		JsonObject data = task.getJsonObject(DistributedVerticle.JSON_OBJECT);
		cache.safeConsum(unused -> {
			FillModelWithData.saveOcrDataInModel(engine, data);
		});
		future.complete();
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.failed())
			throw new IllegalStateException("Exception in OcrPersistenceVerticle.", res.cause());
	}

}
