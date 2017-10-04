package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;

public class DistributedVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static final String BASE_PATH = System.getenv("HOME") + "/genericsystem/gs-ir-files/";
	public static final String RESOURCES_FOLDER = System.getProperty("user.dir") + "/src/main/resources/";
	protected static final String FILENAME = "filename";
	protected static final String JSON_OBJECT = "jsonObject";
	protected static final String TYPE = "type";
	protected static final String IP = "IP";

	private static final int availProc = Runtime.getRuntime().availableProcessors();
	private static AtomicInteger currentExecutions = new AtomicInteger();

	static {
		logger.debug("Available processors: {} | BASE_PATH: {} | RESOURCES_FOLDER: {}", availProc, BASE_PATH, RESOURCES_FOLDER);
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
		return 1;
		// int instances = availProc / 2;
		// return instances < 1 ? 1 : instances;

	}

	@Override
	public void start() throws Exception {
		vertx.deployVerticle(new PdfConverterVerticle());
		vertx.deployVerticle(new ClassifierVerticle());
		vertx.deployVerticle(new OcrWorkerVerticle());
		vertx.deployVerticle(new DeskewerVerticle());
		vertx.deployVerticle(new ClassifierUsingFieldsVerticle());
		vertx.deployVerticle(new NewClassCreatorVerticle());
		vertx.deployVerticle(new CopyToResourcesVerticle());
		vertx.deployVerticle(new AnnotateImageVerticle());
	}

	public static void main(String[] args) {
		DistributedVerticle verticle = new DistributedVerticle();
		verticle.doDeploy();
	}

	public void doDeploy() {
		Handler<AsyncResult<String>> completionHandler = ar -> {
			if (ar.failed())
				throw new IllegalStateException(ar.cause());
			logger.debug("Deployed {} DistributedVerticle", getMaxExecutions() < 1 ? 1 : getMaxExecutions());
		};

		Tools.deployOnCluster(vertx -> {
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
			});
			vertx.deployVerticle(this, complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
				if (getMaxExecutions() > 1)
					vertx.deployVerticle(DistributedVerticle.class.getName(), new DeploymentOptions().setInstances(getMaxExecutions() - 1), completionHandler);
			});
		});
	}
}
