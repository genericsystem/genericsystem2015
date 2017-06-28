package org.genericsystem.watch.beta;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {

	public static final String PUBLIC_ADDRESS = "publicAdress";
	public final String PRIVATE_ADDRESS = "privateAdress" + hashCode();

	// private final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class, Message.class);
	// private final Cache cache = engine.newCache();
	// private final Generic messageType = engine.find(Message.class);
	// private final Generic taskType = engine.find(Task.class);

	private static final int ATTEMPTS = 5;

	public static void main(String[] args) {

		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost("0.0.0.0");
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
		vertx.eventBus().consumer(PRIVATE_ADDRESS, message -> {
			message.reply("do this task", reply -> {
				if (reply.failed())
					throw new IllegalStateException(reply.cause());
				System.out.println("Receive : " + reply.result().body());
			});
		});

		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			System.out.println("Receive : " + message.body());
			vertx.eventBus().send((String) message.body(), PRIVATE_ADDRESS, reply -> {
				if (reply.failed())
					throw new IllegalStateException(reply.cause());
				System.out.println("Receive : " + reply.result().body());
				reply.result().reply("ok");
			});
		});

		vertx.setPeriodic(3000, l -> {
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});

	}
}
