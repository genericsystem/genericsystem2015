package org.genericsystem.watch.beta;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;
import org.genericsystem.watch.beta.Model.Message;
import org.genericsystem.watch.beta.Model.Task;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {

	public static final String PUBLIC_ADDRESS = "publicAddress";
	public final String PRIVATE_ADDRESS = "privateAddress " + hashCode();

	private final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class, Message.class);
	private final Cache cache = engine.newCache();
	private final Generic messageType = engine.find(Message.class);
	private final Generic taskType = engine.find(Task.class);
	private final RoundRobin roundrobin = new RoundRobin();
	private static final DeliveryOptions TIMEOUT = new DeliveryOptions().setSendTimeout(2000);

	private static final int ATTEMPTS = 5;

	public static void main(String[] args) {

		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost("192.168.1.16");
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);

		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				vertx.deployVerticle(new DistributedVerticle(), result -> {
					System.out.println(result.result());
				});
			} else {
				throw new IllegalStateException(res.cause());
			}
		});
	}

	@Override
	public void start() throws Exception {

		// Periodic aknowledge of availability
		vertx.setPeriodic(5000, h -> {
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});

		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			roundrobin.Register((String) message.body());
		});

		// Periodic : Task creation
		vertx.setPeriodic(10000, m -> {
			cache.safeConsum(n -> {
				messageType.addInstance(new JsonObject().put("task", System.currentTimeMillis()).put("state", "TODO").encodePrettily());
				cache.flush();
			});
		});

		// Periodic : Messages send
		vertx.setPeriodic(5000, l -> {

			cache.safeConsum(n -> {
				Snapshot<Generic> s = messageType.getInstances();
				System.out.println("========================================================================");
				System.out.println(s.toList().toString());
				System.out.println("========================================================================");
				for (Generic message : messageType.getInstances()) {
					JsonObject json = new JsonObject((String) message.getValue());
					if ("TODO".equals(json.getString("state"))) {
						message.remove();
						Generic inProgress = messageType.addInstance(new JsonObject().put("task", json.getLong("task")).put("state", "IN PROGRESS").encodePrettily());
						cache.flush();
						String workerAddress = roundrobin.getNextAddress();
						if (workerAddress != null) {
							vertx.eventBus().send(workerAddress, inProgress.getValue(), TIMEOUT, reply -> {
								cache.safeConsum(nothing -> {
									inProgress.remove();
									if (reply.failed()) {
										System.out.println(reply.cause());
										roundrobin.remove(workerAddress);
										messageType.addInstance(new JsonObject().put("task", new JsonObject((String) inProgress.getValue()).getLong("task")).put("state", "TODO").encodePrettily());

									} else {
										if ("KO".equals(reply.result().body()))
											messageType.addInstance(new JsonObject().put("task", new JsonObject((String) inProgress.getValue()).getLong("task")).put("state", "TODO").encodePrettily());

									}
									cache.flush();
								});
							});
						} else {
							System.out.println("No worker Verticle available");
						}

					}
				}
			});

		});

		// Messages handling

		vertx.eventBus().consumer(PRIVATE_ADDRESS, message -> {
			System.out.println("---------------------------------------------------------------");
			System.out.println("task received " + message.body());
			// traitement
			message.reply("KO");
		});

	}

}
