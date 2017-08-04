package org.genericsystem.watch;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class OcrEngineHolderVerticle extends AbstractVerticle {

	private final Root engine;

	public OcrEngineHolderVerticle(Root engine) {
		this.engine = engine;
	}

	public static void main(String[] args) {
		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost(LocalNet.getIpAddress());
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			Vertx vertx = res.result();
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
			});
			vertx.deployVerticle(new OcrEngineHolderVerticle(FillModelWithData.getEngine()), result -> {
				if (result.failed())
					throw new IllegalStateException(result.cause());
			});
		});
	}

	public void doDeploy() {
		ClusterManager mgr = new HazelcastClusterManager();
		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost(LocalNet.getIpAddress());
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			Vertx vertx = res.result();
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
