package org.genericsystem.ir.gui.utils;

import org.genericsystem.common.GSVertx;
import org.genericsystem.common.Root;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;

/**
 * This class extends {@link AbstractVerticle} in order to provide a worker verticle. One must override the default {@link #start()} or {@link #start(io.vertx.core.Future)} methods.
 * 
 * @author Pierrik Lassalas
 */
@SuppressWarnings("unused")
public class WorkerVerticle extends AbstractVerticle {

	private final Root root;
	private static final DeploymentOptions OPTIONS_NORMAL = new DeploymentOptions();
	private static final DeploymentOptions OPTIONS_WORKER = new DeploymentOptions().setWorker(true).setMaxWorkerExecuteTime(Long.MAX_VALUE);

	public WorkerVerticle() {
		this(null);
	}

	public WorkerVerticle(Root root) {
		this.root = root;
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		System.out.println("Worker thread: " + Thread.currentThread().getName());
		start();
		startFuture.complete();
	}

	@Override
	public void start() throws Exception {
		throw new IllegalStateException("You need to implement the start() method of the worker verticle.");
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
		});
	}
}
