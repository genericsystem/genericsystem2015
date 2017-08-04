package org.genericsystem.watch;

import java.util.concurrent.atomic.AtomicInteger;

import io.vertx.core.AbstractVerticle;

public class DistributedVerticle extends AbstractVerticle {
	public static final String BASE_PATH = System.getenv("HOME") + "/git/genericsystem2015/gs-cv/";
	protected static final String FILENAME = "filename";
	protected static final String JSON_OBJECT = "jsonObject";
	protected static final String TYPE = "type";
	protected static final String IP = "IP";

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

	@Override
	public void start() throws Exception {
		vertx.deployVerticle(new PdfConverterVerticle());
		vertx.deployVerticle(new ClassifierVerticle());
		vertx.deployVerticle(new OcrWorkerVerticle());
	}

	public static void main(String[] args) {
		Tools.deployOnCluster(vertx -> {
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
				vertx.deployVerticle(new DistributedVerticle(), result -> {
					if (result.failed())
						throw new IllegalStateException(result.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(), result -> {
					if (result.failed())
						throw new IllegalStateException(result.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(), result -> {
					if (result.failed())
						throw new IllegalStateException(result.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(), result -> {
					if (result.failed())
						throw new IllegalStateException(result.cause());
				});
			});
		});
	}
}
