package org.genericsystem.watch;

import org.genericsystem.common.GSVertx;
import org.genericsystem.common.Root;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Verticle;

/**
 * This class allow the deployment of a series of verticles from a gs-reactor application (WatchApp).
 * 
 * @author Pierrik Lassalas
 */
public class VerticleDeployerFromWatchApp extends AbstractVerticle {

	private static final DeploymentOptions OPTIONS_NORMAL = new DeploymentOptions();
	private static final DeploymentOptions OPTIONS_WORKER = new DeploymentOptions().setWorker(true).setMaxWorkerExecuteTime(Long.MAX_VALUE);
	private Root root;

	/**
	 * Constructor of this class.
	 * 
	 * @param root - the engine (from the application deployed with gs-reactor)
	 */
	public VerticleDeployerFromWatchApp(Root root) {
		this.root = root;
	}

	/**
	 * Deploy a single worker verticle.
	 * 
	 * @param verticle - the verticle to deploy
	 * @param errorMessage - the error message that will be displayed if the deployment fails
	 * @throws IllegalStateException when the verticle can not be deployed
	 */
	public static void deployWorkerVerticle(Verticle verticle, String errorMessage) throws IllegalStateException {
		GSVertx.vertx().getVertx().deployVerticle(verticle, OPTIONS_WORKER, res -> {
			if (res.failed())
				throw new IllegalStateException(errorMessage != null ? errorMessage : "Deployment of worker verticle failed.", res.cause());
		});
	}

	/**
	 * Start the deployment of all other verticles from the GUI interface.
	 */
	public void doDeploy() {
		GSVertx.vertx().getVertx().deployVerticle(this, OPTIONS_WORKER, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of main verticle (" + deploymentID() + ") failed.", res.cause());
			else
				System.out.println("Main verticle (" + deploymentID() + ") deployed");
		});
	}

	private void deployVerticle(Verticle verticle, boolean worker) throws IllegalStateException {
		vertx.deployVerticle(verticle, worker ? OPTIONS_WORKER : OPTIONS_NORMAL, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticle failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		deployVerticle(new Dispatcher(), true);
		deployVerticle(new DistributedVerticle(), true);
		deployVerticle(new OcrEngineHolderVerticle(root), true);
		startFuture.complete();
	}
}
