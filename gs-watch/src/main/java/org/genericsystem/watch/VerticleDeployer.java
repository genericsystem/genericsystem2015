package org.genericsystem.watch;

import java.nio.file.Paths;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Verticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class VerticleDeployer extends AbstractVerticle {

	public static final String PDF_WATCHER_ADDRESS = "app.pdfchanges";
	public static final String PNG_WATCHER_ADDRESS = "app.pngchanges";
	public static final String IMAGE_ADDED_TO_CLASS_ADDRESS = "app.class.newimage";

	public static void deployVerticle(Verticle verticle) {
		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);

		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				vertx.deployVerticle(verticle, new DeploymentOptions());
			} else {
				throw new IllegalStateException(res.cause());
			}
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		DeploymentOptions options = new DeploymentOptions().setWorker(true);
		vertx.deployVerticle(new DirectoryWatcherVerticle(Paths.get("..", "gs-cv", "pdf"), PDF_WATCHER_ADDRESS), options);
		vertx.deployVerticle(new PdfsConverterVerticle(), options);
		vertx.deployVerticle(new ClassifierVerticle(), options);
		vertx.deployVerticle(new RunScriptVerticle());
		startFuture.complete();
	}
}
