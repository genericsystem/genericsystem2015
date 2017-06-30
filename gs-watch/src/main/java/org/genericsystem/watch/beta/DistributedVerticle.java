package org.genericsystem.watch.beta;

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

	private final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class,
			Message.class);
	private final Cache cache = engine.newCache();
	private final Generic messageType = engine.find(Message.class);
	private final Generic taskType = engine.find(Task.class);
	private final RoundRobin roundrobin = new RoundRobin();
	private static final DeliveryOptions TIMEOUT = new DeliveryOptions().setSendTimeout(2000);

	private static final String IP_ADDRESS = "192.168.1.11";
	private static final int ATTEMPTS = 5;

	private static final long AVAILABILITY_PERIODICITY = 5000;
	private static final long TASK_CREATION_PERIODICITY = 1000;
	private static final long MESSAGE_SEND_PERIODICITY = 1000;

	private static final String TODO = "TODO";
	private static final String INPROGRESS = "IN PROGRESS";

	private static final String STARTED = "started";
	private static final String FINISHED = "finished";
	private static final String ABORTED = "aborted";

	private static final String OK = "OK";
	private static final String KO = "KO";

	private int nb_executions;

	public static void main(String[] args) {

		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost(IP_ADDRESS);
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

		cache.safeConsum(nothing -> {
			for (Generic task1 : taskType.getInstances()) {
				if (STARTED.equals(new JsonObject((String) task1.getValue()).getString("state"))) {
					messageType.addInstance(
							new JsonObject().put("task", new JsonObject((String) task1.getValue()).getLong("task"))
									.put("state", TODO).put("max_parallel_executions", 5).encodePrettily());
					task1.remove();
				}
			}

			for (Generic message1 : messageType.getInstances()) {
				if (INPROGRESS.equals(new JsonObject((String) message1.getValue()).getString("state"))) {
					messageType.addInstance(
							new JsonObject().put("task", new JsonObject((String) message1.getValue()).getLong("task"))
									.put("state", TODO).put("max_parallel_executions", 5).encodePrettily());
					message1.remove();
				}
			}
			cache.flush();
		});

		// Periodic aknowledge of availability
		vertx.setPeriodic(AVAILABILITY_PERIODICITY, h -> {
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});

		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			roundrobin.Register((String) message.body());
		});

		// Periodic : Task creation
		vertx.setPeriodic(TASK_CREATION_PERIODICITY, m -> {
			cache.safeConsum(n -> {
				messageType.addInstance(new JsonObject().put("task", System.currentTimeMillis()).put("state", TODO)
						.put("max_parallel_executions", 5).encodePrettily());
				cache.flush();
			});
		});

		// Periodic : Messages send
		vertx.setPeriodic(MESSAGE_SEND_PERIODICITY, l -> {

			cache.safeConsum(n -> {
				System.out.println("========================================================================");
				System.out.println(messageType.getInstances().toList().toString());
				System.out.println("------------------------------------------------------------------------");
				System.out.println(taskType.getInstances().toList().toString());
				System.out.println("========================================================================");
				for (Generic message : messageType.getInstances()) {
					JsonObject json = new JsonObject((String) message.getValue());
					if (TODO.equals(json.getString("state"))) {

						String workerAddress = roundrobin.getNextAddress();
						if (workerAddress != null) {
							message.remove();
							Generic inProgress = messageType.addInstance(
									new JsonObject().put("task", json.getLong("task")).put("state", INPROGRESS)
											.put("max_parallel_executions", 5).encodePrettily());
							cache.flush();
							vertx.eventBus().send(workerAddress, inProgress.getValue(), TIMEOUT, reply -> {
								cache.safeConsum(nothing -> {
									inProgress.remove();
									if (reply.failed()) {
										System.out.println(reply.cause());
										roundrobin.remove(workerAddress);
										messageType
												.addInstance(new JsonObject()
														.put("task",
																new JsonObject((String) inProgress.getValue())
																		.getLong("task"))
														.put("state", TODO).put("max_parallel_executions", 5)
														.encodePrettily());
									} else {
										if (KO.equals(reply.result().body()))
											messageType
													.addInstance(
															new JsonObject()
																	.put("task",
																			new JsonObject(
																					(String) inProgress.getValue())
																							.getLong("task"))
																	.put("state", TODO)
																	.put("max_parallel_executions", 5)
																	.encodePrettily());
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

			cache.safeConsum(nothing -> {

				nb_executions = 0;
				for (Generic task : taskType.getInstances())
					if (STARTED.equals(new JsonObject((String) task.getValue()).getString("state"))) {
						nb_executions++;
					}

				if (new JsonObject((String) message.body()).getInteger("max_parallel_executions") <= nb_executions) {
					message.reply(KO);
					return;
				}

				long taskTs = new JsonObject((String) message.body()).getLong("task");
				String messageTask = new JsonObject().put("task", taskTs).put("state", STARTED).encodePrettily();
				taskType.addInstance(messageTask);
				cache.flush();
				vertx.executeBlocking(future -> {
					try {
						System.out.println("executing task : " + messageTask.toString());
						Thread.sleep(10000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}

					future.complete();
				}, res -> {
					cache.safeConsum(nothing2 -> {
						taskType.getInstance(messageTask).remove();
						if (res.succeeded()) {
							taskType.addInstance(
									new JsonObject().put("task", taskTs).put("state", FINISHED).encodePrettily());

						} else
							taskType.addInstance(
									new JsonObject().put("task", taskTs).put("state", ABORTED).encodePrettily());
						cache.flush();
					});

				});
				message.reply(OK);
			});

		});

	}

}
