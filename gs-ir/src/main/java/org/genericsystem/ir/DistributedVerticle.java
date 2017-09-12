package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

public class DistributedVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static final String BASE_PATH = System.getenv("HOME") + "/genericsystem/gs-ir-files/";
	protected static final String FILENAME = "filename";
	protected static final String JSON_OBJECT = "jsonObject";
	protected static final String TYPE = "type";
	protected static final String IP = "IP";

	private static final int availProc = Runtime.getRuntime().availableProcessors();
	private static AtomicInteger currentExecutions = new AtomicInteger();

	static {
		logger.debug("Available processors: {}", availProc);
	}

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
		int instances = availProc / 2;
		return instances < 1 ? 1 : instances;
	}

	@Override
	public void start() throws Exception {
		vertx.deployVerticle(new PdfConverterVerticle());
		vertx.deployVerticle(new ClassifierVerticle());
		vertx.deployVerticle(new OcrWorkerVerticle());
	}

	public static void main(String[] args) {
		DistributedVerticle verticle = new DistributedVerticle();
		verticle.doDeploy();
	}

	public void doDeploy() {
		Handler<AsyncResult<String>> completionHandler = ar -> {
			if (ar.failed())
				throw new IllegalStateException(ar.cause());
		};

		Tools.deployOnCluster(vertx -> {
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
				// Deploy the current verticle, and additional ones if there are enough resources
				vertx.deployVerticle(this, completionHandler);
				for (int i = 0; i < getMaxExecutions() - 1; ++i) {
					vertx.deployVerticle(new DistributedVerticle(), completionHandler);
				}
				logger.debug("Deployed {} DistributedVerticle", getMaxExecutions() < 1 ? 1 : getMaxExecutions());
			});
		});
	}
}
