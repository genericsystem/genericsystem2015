package org.genericsystem.watch;

import java.util.concurrent.atomic.AtomicInteger;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {
	public static final String BASE_PATH = System.getenv("HOME") + "/git/genericsystem2015/gs-cv/";
	protected static final String FILENAME = "filename";
	protected static final String TYPE = "type";
	protected static final String IP = "IP";

	private final String ip;

	private static AtomicInteger currentExecutions = new AtomicInteger();

	public static void incrementExecutions() {
		currentExecutions.incrementAndGet();
	}

	public static void decrementExecutions() {
		currentExecutions.decrementAndGet();
	}

	public static int getExecutionsCount() {
		return currentExecutions.intValue();
	}

	public static int getMaxExecutions() {
		// TODO: Add some logic here…
		return 4;
	}

	public DistributedVerticle(String ip) {
		this.ip = ip;
	}

	@Override
	public void start() throws Exception {
		vertx.deployVerticle(new PdfConverterVerticle(ip));
		vertx.deployVerticle(new ClassifierVerticle(ip));
	}

	public static void main(String[] args) {
		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		String ip = LocalNet.getIpAddress();
		vertxOptions.setClusterHost(ip);
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			Vertx vertx = res.result();
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
			});
		});
	}
}
