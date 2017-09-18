package org.genericsystem.ir;

import org.genericsystem.common.Root;
import org.genericsystem.kernel.Cache;

import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public abstract class ActionPersistentVerticle extends ActionVerticle {

	protected Root engine;
	protected Cache cache;

	/**
	 * Default constructor. A reference to the engine must be provided.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public ActionPersistentVerticle(Root engine) {
		this.engine = engine;
		this.cache = (Cache) engine.newCache();
	}

	@Override
	protected Handler<Future<Object>> getExecuteBlockingHandler(JsonObject task) {
		return future -> {
			cache.shiftTs();
			cache.safeConsum(unused -> {
				System.out.println("--- inside executeBlocking (ActionPersistent) + " + Thread.currentThread().getName());
				super.getExecuteBlockingHandler(task).handle(future);
			});
		};
	}
}
