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
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {

	public static final String PUBLIC_ADDRESS = "publicAdress";
	public final String PRIVATE_ADDRESS = "privateAdress" + hashCode();

	private final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class, Message.class);
	private final Cache cache = engine.newCache();
	private final Generic messageType = engine.find(Message.class);
	private final Generic taskType = engine.find(Task.class);

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
		vertx.eventBus().consumer(PRIVATE_ADDRESS, jobAsk -> {
			Generic gMessage = getNextMessage();
			if (gMessage != null) {
				jobAsk.reply(gMessage.getValue(), reply -> {
					if (reply.failed())
						throw new IllegalStateException(reply.cause());
					System.out.println("Receive : " + reply.result().body());
					cache.safeConsum(n -> {
						if ("OK".equals(reply.result().body()))
							gMessage.remove();
						else
							gMessage.updateValue(new JsonObject().put("task", new JsonObject((String) gMessage.getValue()).getLong("task")).put("state", "TODO").encodePrettily());
						cache.flush();
					});

				});
			} else {
				jobAsk.reply("NOTHING");
			}

		});

		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			vertx.eventBus().send((String) message.body(), "jobAsk", reply -> {
				if (reply.failed())
					throw new IllegalStateException(reply.cause());
				System.out.println("Receive : " + reply.result().body());
				if ("NOTHING".equals(reply.result().body()))
					return;
				reply.result().reply("KO");
			});
		});

		vertx.setPeriodic(3000, l -> {
			cache.safeConsum(n -> {
				System.out.println("=====================================================");
				Snapshot<Generic> messages = messageType.getInstances();
				System.out.println(messages.toList().toString());
				System.out.println("=====================================================");
			});

			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});

		vertx.setTimer(10000, l -> {
			cache.safeConsum(n -> {
				messageType.addInstance(new JsonObject().put("task", System.currentTimeMillis()).put("state", "TODO").encodePrettily());
				cache.flush();
			});

		});

	}

	private Generic getNextMessage() {
		return cache.safeSupply(() -> {
			Snapshot<Generic> s = messageType.getInstances();
			System.out.println(s.toList().toString());
			for (Generic result : messageType.getInstances()) {
				JsonObject json = new JsonObject((String) result.getValue());
				if ("TODO".equals(json.getString("state"))) {
					result = result.updateValue(new JsonObject().put("task", json.getLong("task")).put("state", "IN PROGRESS").encodePrettily());
					cache.flush();
					return result;
				}
			}
			return null;
		});

	}
}
