package org.genericsystem.watch;

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
	public static final String NEW_IMAGE_ADDED_TO_CLASS = "app.class.newimage.added";
	public static final String ACCURATE_ZONES_NOT_FOUND = "app.class.nozone";
	public static final String ACCURATE_ZONES_FOUND = "app.class.zone";
	public static final String IMAGE_TO_OCR = "app.ocr.newimage";
	public static final String NEW_IMAGE_PROCESSED = "app.ocr.newimage.processed";
	public static final String KNOWN_IMAGE_PROCESSED = "app.ocr.knownimage.updated";
	public static final String KNOWN_IMAGE_PASSED = "app.ocr.knownimage.passed";

	public static void deployVerticle(Verticle verticle) {
		deployVerticle(verticle, new VertxOptions());
	}

	public static void deployVerticle(Verticle verticle, VertxOptions vertxOptions) {
		ClusterManager mgr = new HazelcastClusterManager();
		vertxOptions.setClustered(true).setClusterManager(mgr);
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
		vertx.deployVerticle(new MailWatcherVerticle());
		vertx.deployVerticle(new PdfsConverterVerticle());
		vertx.deployVerticle(new ClassifierVerticle());
		vertx.deployVerticle(new DezonerVerticle());
		vertx.deployVerticle(new OcrVerticle());
		startFuture.complete();
	}
}
