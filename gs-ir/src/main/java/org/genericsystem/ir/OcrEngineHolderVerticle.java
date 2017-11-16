package org.genericsystem.ir;

import org.genericsystem.common.Root;
import org.genericsystem.ir.AbstractMultitonVerticle.AbstractSingletonVerticle;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class OcrEngineHolderVerticle extends AbstractSingletonVerticle {

	private final Root engine;
	private static final String COUNTER = OcrEngineHolderVerticle.class.getName();

	public OcrEngineHolderVerticle(Root engine) {
		this.engine = engine;
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		Handler<AsyncResult<String>> completionHandler = ar -> {
			if (ar.failed())
				startFuture.fail(ar.cause());
		};
		vertx.deployVerticle(new AddImageToEngineVerticle(engine), completionHandler);
		vertx.deployVerticle(new DezonerVerticle(engine), completionHandler);
		// vertx.deployVerticle(new OcrParametersVerticle(engine), completionHandler);
		vertx.deployVerticle(new OcrPersistenceVerticle(engine), completionHandler);
		vertx.deployVerticle(new LinkImgToDocClassVerticle(engine), completionHandler);
	}

	@Override
	protected void deployVerticle(Vertx vertx) {
		vertx.deployVerticle(new HttpServerVerticle(), complete -> {
			if (complete.failed())
				throw new IllegalStateException(complete.cause());
		});
		vertx.deployVerticle(this, result -> {
			if (result.failed())
				throw new IllegalStateException("Deployment of OcrEngineHolderVerticle failed", result.cause());
		});
	}

	@Override
	protected String getCounter() {
		return COUNTER;
	}
}
