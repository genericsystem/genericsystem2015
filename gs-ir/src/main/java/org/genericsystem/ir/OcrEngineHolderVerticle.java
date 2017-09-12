package org.genericsystem.ir;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class OcrEngineHolderVerticle extends AbstractVerticle {

	private final Root engine;

	public OcrEngineHolderVerticle(Root engine) {
		this.engine = engine;
	}

	public static void main(String[] args) {
		OcrEngineHolderVerticle verticle = new OcrEngineHolderVerticle(FillModelWithData.getEngine());
		verticle.doDeploy();
	}

	public void doDeploy() {
		Tools.deployOnCluster(vertx -> {
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
			});
			vertx.deployVerticle(this, result -> {
				if (result.failed())
					throw new IllegalStateException("Deployment of OcrEngineHolderVerticle failed", result.cause());
			});
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		Handler<AsyncResult<String>> completionHandler = ar -> {
			if (ar.failed())
				startFuture.fail(ar.cause());
		};
		vertx.deployVerticle(new AddImageToEngineVerticle(engine), completionHandler);
		vertx.deployVerticle(new DezonerVerticle(engine), completionHandler);
		vertx.deployVerticle(new OcrParametersVerticle(engine), completionHandler);
		vertx.deployVerticle(new OcrPersistenceVerticle(engine), completionHandler);
	}
}
