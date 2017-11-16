package org.genericsystem.ir.app.gui.utils;

import java.lang.invoke.MethodHandles;

import org.genericsystem.common.GSVertx;
import org.genericsystem.common.Root;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;

/**
 * This class extends {@link AbstractVerticle} in order to provide a worker verticle. One must override the default {@link #start()} or {@link #start(io.vertx.core.Future)} methods.
 */
@SuppressWarnings("unused")
public abstract class WorkerVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final DeploymentOptions OPTIONS_NORMAL = new DeploymentOptions();
	private static final DeploymentOptions OPTIONS_WORKER = new DeploymentOptions().setWorkerPoolName("gs-ir-worker-verticle").setWorkerPoolSize(5).setMultiThreaded(false).setMaxWorkerExecuteTime(Long.MAX_VALUE).setWorker(true);

	private final Root root;

	public WorkerVerticle() {
		this(null);
	}

	public WorkerVerticle(Root root) {
		this.root = root;
	}

	/**
	 * Deploy this verticle as a worker.
	 * 
	 * @param errorMessage - the error message that will be displayed if the deployment fails
	 * @throws IllegalStateException when the verticle can not be deployed
	 */
	public void deployAsWorkerVerticle(String errorMessage) throws IllegalStateException {
		GSVertx.vertx().getVertx().deployVerticle(this, OPTIONS_WORKER, res -> {
			if (res.failed())
				throw new IllegalStateException(errorMessage != null ? errorMessage : "Deployment of worker verticle failed.", res.cause());
			else {
				undeployVerticle(res.result());
			}
		});
	}

	private void undeployVerticle(String deploymentId) {
		vertx.undeploy(deploymentId, res -> {
			if (res.failed())
				throw new IllegalStateException("Failed to undeploy worker verticle", res.cause());
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		System.out.println("Worker thread: " + Thread.currentThread().getName());
		try {
			start();
		} catch (Exception e) {
			logger.warn("An exception was caught during start. Please see the logs for details.", e);
		}
		startFuture.complete();
	}

	/**
	 * Defines the action(s) that need to be executed in a separate thread to prevent blocking.
	 */
	@Override
	public abstract void start();

}
