package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.shareddata.Counter;
import io.vertx.core.shareddata.SharedData;

/**
 * This class is used to deploy only a specific number of instance of a Verticle on a cluster. If a second instance of the Verticle is deployed, the deployment will be rolled back.
 */
public abstract class AbstractMultitonVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected final String className = this.getClass().getSimpleName();
	protected final String startingErrorMsg = "An error has occured while deploying" + className;
	protected final String stoppingErrorMsg = "An error has occured while stopping" + className;
	protected boolean isDeployed;

	/**
	 * This method returns the name of the cluster-wide counter. This name should be unique.
	 * 
	 * @return a String
	 */
	protected abstract String getCounter();

	/**
	 * This method returns the expected maximum value of the cluster-wide counter. If represents the maximum number of instances for the Verticle extending this class.
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
					throw new IllegalStateException(startingErrorMsg, res.cause());
				} else {
					Counter counter = res.result();
					logger.debug("Couter {} successfully acquired", getCounter());
					counter.incrementAndGet(ar -> {
						if (!ar.succeeded())
							throw new IllegalStateException(startingErrorMsg, res.cause());
						else {
							long value = ar.result();
							logger.debug("Counter {} incremented to {} (previously {})", getCounter(), value, (value - 1));
							if (value == getCounterOk()) {
								logger.debug("Deploying verticle...");
								deployVerticle(vertx);
								this.isDeployed = true;
								logger.debug("Verticle deployed!");
							} else {
								logger.warn("An instance of {} is already deployed on the cluster. Aborting...", className);
								counter.decrementAndGet(ar2 -> {
									if (!ar2.succeeded())
										throw new IllegalStateException(startingErrorMsg, res.cause());
									else {
										long newValue = ar2.result();
										logger.debug("Counter {} decremented to {} (previously {})", getCounter(), newValue, (newValue + 1));
										undeployVerticle(vertx);
									}
								});
							}
						}
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

	@Override
	public void stop(Future<Void> stopFuture) throws Exception {
		if (this.isDeployed) {
			SharedData sd = vertx.sharedData();
			sd.getCounter(getCounter(), res -> {
				if (!res.succeeded()) {
					throw new IllegalStateException(stoppingErrorMsg, res.cause());
				} else {
					Counter counter = res.result();
					logger.debug("Couter {} successfully acquired", getCounter());
					counter.get(ar -> {
						if (!ar.succeeded())
							throw new IllegalStateException(stoppingErrorMsg, res.cause());
						else {
							long value = ar.result();
							logger.debug("Counter {} has value: {})", getCounter(), value);
							if (value == getCounterOk())
								throw new IllegalStateException(String.format("Unexpected value (%d) for counter %s", value, getCounter()));
							else {
								counter.decrementAndGet(ar2 -> {
									if (!ar2.succeeded())
										throw new IllegalStateException(stoppingErrorMsg, res.cause());
									else {
										long newValue = ar2.result();
										logger.debug("Counter {} decremented to {} (previously {})", getCounter(), newValue, (newValue + 1));
										this.isDeployed = false;
									}
								});
							}
						}
					});
				}
			});
		}
		super.stop(stopFuture);
	}

	/**
	 * Deploys only a single instance of a Verticle.
	 */
	public static abstract class AbstractSingletonVerticle extends AbstractMultitonVerticle {
		@Override
		protected int getCounterOk() {
			return 1;
		}
	}
}
