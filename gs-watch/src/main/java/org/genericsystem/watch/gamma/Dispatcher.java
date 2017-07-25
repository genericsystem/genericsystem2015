package org.genericsystem.watch.gamma;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;
import org.genericsystem.watch.beta.Model.Task;
import org.genericsystem.watch.beta.RoundRobin;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class Dispatcher extends AbstractVerticle {

	protected final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/tasks/", Task.class);
	protected Cache cache = engine.newCache();
	protected final Generic taskType = engine.find(Task.class);
	private final RoundRobin roundRobin = new RoundRobin();

	private static final String OK = "OK";
	private static final String KO = "KO";

	public static final String ADDRESS = "org.genericsystem.repartitor";
	protected static final String STATE = "state";
	protected static final String TODO = "todo";
	private static final String INPROGRESS = "IN PROGRESS";
	private static final String STARTED = "started";
	protected static final String FINISHED = "finished";
	protected static final String ABORTED = "aborted";
	private static final long MESSAGE_SEND_PERIODICITY = 5000;
	private static final DeliveryOptions TIMEOUT = new DeliveryOptions().setSendTimeout(2000);

	public Dispatcher(String ip) {
		cache.safeConsum(nothing -> {
			taskType.addInstance(new JsonObject().put(STATE, TODO).put(DistributedVerticle.FILENAME, "pdf/image.pdf").put(DistributedVerticle.IP, ip).put(DistributedVerticle.TYPE, DownloadVerticle.ACTION).encodePrettily());
			taskType.addInstance(new JsonObject().put(STATE, TODO).put(DistributedVerticle.FILENAME, "pdf/image2.pdf").put(DistributedVerticle.IP, ip).put(DistributedVerticle.TYPE, DownloadVerticle.ACTION).encodePrettily());
			taskType.addInstance(new JsonObject().put(STATE, TODO).put(DistributedVerticle.FILENAME, "pdf/image3.pdf").put(DistributedVerticle.IP, ip).put(DistributedVerticle.TYPE, DownloadVerticle.ACTION).encodePrettily());
			cache.flush();
		});
	}

	public static void main(String[] args) {
		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		String ip = LocalNet.getIpAddress();
		vertxOptions.setClusterHost(ip);
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			Vertx vertx = res.result();
			vertx.deployVerticle(new Dispatcher(ip), res_ -> {
				if (res_.failed())
					throw new IllegalStateException(res_.cause());
			});
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		vertx.eventBus().consumer(DistributedVerticle.PUBLIC_ADDRESS, message -> {
			roundRobin.register((String) message.body());
		});
		vertx.eventBus().consumer(ADDRESS + ":add", message -> {
			System.out.println("Add consumer: " + message.body());
			cache.safeConsum(unused -> {
				taskType.addInstance((String) message.body());
				cache.flush();
			});
		});
		vertx.setPeriodic(MESSAGE_SEND_PERIODICITY, h -> {
			cache.safeConsum(unused -> {
				for (Generic task : taskType.getInstances()) {
					JsonObject json = new JsonObject((String) task.getValue());
					if (TODO.equals(json.getString(STATE))) {
						String robin = roundRobin.getNextAddress();
						System.out.println("task: " + task + ", adress: " + robin);
						if (robin != null) {
							vertx.eventBus().send(robin + ":" + json.getString(DistributedVerticle.TYPE), json.encodePrettily(), TIMEOUT, reply -> {
								if (reply.failed()) {
									roundRobin.remove(robin);
									throw new IllegalStateException(reply.cause());
								} else if (OK.equals(reply.result().body()))
									cache.safeConsum(unused_ -> task.remove());
							});
						}
					}
				}
				cache.flush();
			});
		});
		startFuture.complete();
	}
}
