package org.genericsystem.watch.beta;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;
import org.genericsystem.watch.beta.Verticle.Model.Message;
import org.genericsystem.watch.beta.Verticle.Model.Task;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class Verticle extends AbstractVerticle {

	public static final String BUS_MSG = "app.Message";
	// public static final String BUS_TASK = "app.Task";
	private final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class,
			Message.class);
	private final Cache cache = engine.newCache();
	private final Generic messageType = engine.find(Message.class);
	private final Generic taskType = engine.find(Task.class);
	private static final int ATTEMPTS = 5;

	public static void main(String[] args) {

		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost("192.168.1.11");
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);

		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				vertx.deployVerticle(new Verticle(), result -> {
					System.out.println(result.result());
				});
			} else {
				throw new IllegalStateException(res.cause());
			}
		});

	}

	@Override
	public void start() throws Exception {

		MessageConsumer<String> consumer = vertx.eventBus().consumer(BUS_MSG);

		consumer.handler((event) -> {

			JsonObject obj = new JsonObject(event.body());
			// System.out.println(new JsonObject(event.body()));
			cache.safeConsum(nothing -> {

				String messageTask = new JsonObject().put("messagets", obj.getLong("ts")).put("state", "started")
						.encodePrettily();
				taskType.addInstance(messageTask);
				cache.flush();

				vertx.executeBlocking(future -> {

					try {
						Thread.sleep(10000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}, res -> {
					cache.safeConsum(nothing2 -> {
						taskType.getInstance(messageTask).remove();
						if (res.succeeded()) {

							taskType.addInstance(new JsonObject().put("messagets", obj.getLong("ts"))
									.put("state", "finished").encodePrettily());
						} else {

							taskType.addInstance(new JsonObject().put("messagets", obj.getLong("ts"))
									.put("state", "aborted").encodePrettily());

						}
						cache.flush();
					});

				});
			});

		});

		trySend(new JsonObject().put("ts", System.currentTimeMillis()), false);

		vertx.setPeriodic(3000, l -> {
			System.out.println("Periodic messages check");
			cache.safeConsum(nothing -> {

				Snapshot<Generic> s = messageType.getInstances();
				System.out.println(s.toList().toString());

				s.forEach(g -> {
					System.out.println("Found message :" + g.getValue());
					System.out.println("Try to resend");
					trySend(new JsonObject((String) g.getValue()), true);
				});

				Snapshot<Generic> s1 = taskType.getInstances();
				System.out.println(s.toList().toString());
				s1.forEach(t -> {
					System.out.println("Found task :" + t.getValue());

				});

			});

		});

	}

	public void trySend(JsonObject jsonMessage, boolean persisted) {

		String message = jsonMessage.encodePrettily();
		vertx.eventBus().send(BUS_MSG, message, (event) -> {
			if (event.failed()) {
				System.out.println("failed to deliver message");
				cache.safeConsum(nothing -> {
					if (!persisted) {
						System.out.println("Saving message that can't be sent : " + message);
						messageType.addInstance(message);
						cache.flush();
					} else {
						System.out.println("Message already saved" + message);
					}
				});
			} else {
				if (persisted) {
					System.out.println("Remove message after resend");
					messageType.getInstance(message).remove();
					cache.flush();
				}
			}
		});
	}

	public static class Model {

		@SystemGeneric
		public static class Task {
		}

		@SystemGeneric
		@InstanceValueClassConstraint(String.class)
		public static class Message {
		}

	}
}
