package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.shareddata.Counter;
import io.vertx.core.shareddata.SharedData;

/**
 * This class is used to deploy only a specific number of instance of a Verticle on a cluster. If a second instance of the Verticle is deployed, the deployment will be rolled back.
 * 
 * @author Pierrik Lassalas
 */
public abstract class AbstractMultitonVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * This method returns the name of the cluster-wide counter. This name should be unique.
	 * 
	 * @return a String
	 */
	protected abstract String getCounter();

	/**
	 * This method returns the value of the cluster-wide counter. If represents the maximum number of instances for the Verticle extending this class.
	 * 
	 * @return a String
	 */
	protected abstract int getCounterOk();

	/**
	 * Try to deploy on cluster. If it succeed, the method {@link #deployVerticle(Vertx)} will be called, and {@link #undeployVerticle(Vertx)} otherwise.
	 */
	public void doDeploy() {
		Tools.deployOnCluster(vertx -> {
			SharedData sd = vertx.sharedData();
			sd.getCounter(getCounter(), res -> {
				if (!res.succeeded()) {
					throw new IllegalStateException("An error has occured while deploying" + this.getClass().getSimpleName(), res.cause());
				} else {
					Counter counter = res.result();
					logger.debug("Couter {} successfully acquired", getCounter());
					counter.incrementAndGet(ar -> {
						if (ar.succeeded()) {
							long value = ar.result();
							logger.debug("Counter {} incremented to {} (previously {})", getCounter(), value, (value - 1));
							if (value == getCounterOk()) {
								logger.debug("Deploying verticle...");
								deployVerticle(vertx);
								logger.debug("Verticle deployed!");
							} else {
								logger.warn("An instance of {} is already deployed on the cluster. Aborting...", this.getClass().getSimpleName());
								counter.decrementAndGet(ar2 -> {
									if (ar2.succeeded()) {
										long newValue = ar2.result();
										logger.debug("Counter {} decremented to {} (previously {})", getCounter(), newValue, (newValue + 1));
										undeployVerticle(vertx);
									} else
										throw new IllegalStateException("An error has occured while deploying" + this.getClass().getSimpleName(), res.cause());
								});
							}
						} else
							throw new IllegalStateException("An error has occured while deploying" + this.getClass().getSimpleName(), res.cause());
					});
				}
			});
		});
	}

	/**
	 * This method defines the action to run once the Verticle is deployed (e.g., vertx.deployVerticle(...)).
	 * 
	 * @param vertx - the vertx
	 */
	protected abstract void deployVerticle(Vertx vertx);

	/**
	 * This method defines the action to run in case the Verticle can not be deployed.
	 * 
	 * @param vertx - the vertx
	 */
	protected void undeployVerticle(Vertx vertx) {
		logger.info("Undeploying verticle...");
		vertx.close(res -> {
			if (res.succeeded())
				logger.info("Undeploy successful");
			else
				throw new IllegalStateException("An error has occured while shutting down the verticle", res.cause());
		});
	}

	/**
	 * Deploys only a single instance of a Verticle.
	 * 
	 * @author Pierrik Lassalas
	 */
	public static abstract class AbstractSingletonVerticle extends AbstractMultitonVerticle {
		@Override
		protected int getCounterOk() {
			return 1;
		}
	}
}
