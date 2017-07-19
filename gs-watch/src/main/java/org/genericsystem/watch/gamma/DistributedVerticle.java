package org.genericsystem.watch.gamma;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.watch.beta.RoundRobin;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {
	private static final long REGISTER_PERIODICITY = 1000;
	private static final long ROUNDROBIN_PERIODICITY = 5000;
	private static final String PUBLIC_ADDRESS = "publicAddress";
	public static final String BASE_PATH = System.getenv("HOME") + "/git/genericsystem2015/gs-cv/";
	protected static final String FILENAME = "filename";
	protected static final String OK = "OK";
	private static final String KO = "KO";
	protected static final String ID = "ID";
	protected static final String TYPE = "type";
	protected static final String IP = "IP";
	private final String PRIVATE_ADDRESS;
	private final String PRIVATE_PATH;
	private static final DeliveryOptions TIMEOUT = new DeliveryOptions().setSendTimeout(500);
	private final RoundRobin roundrobin = new RoundRobin();
	private List<JsonObject> messages = new ArrayList<>();
	private List<JsonObject> tasks = new ArrayList<>();

	private final String ip;

	private DistributedVerticle(String ip) {
		this.ip = ip;
		this.PRIVATE_ADDRESS = ip + ":" + hashCode();
		this.PRIVATE_PATH = System.getenv("HOME") + "/copy/" + PRIVATE_ADDRESS + "/";
		System.out.println("Ip : " + ip);

		long id = System.currentTimeMillis();
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image.pdf").put(IP, ip).put(TYPE, DownloadVerticle.ACTION)));
		id = System.currentTimeMillis();
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image2.pdf").put(IP, ip).put(TYPE, DownloadVerticle.ACTION)));
		id = System.currentTimeMillis();
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image3.pdf").put(IP, ip).put(TYPE, DownloadVerticle.ACTION)));
	}

	@Override
	public void start() throws Exception {
		vertx.deployVerticle(new DownloadVerticle(PRIVATE_ADDRESS, PRIVATE_PATH, ip, messages, tasks));
		vertx.deployVerticle(new PdfConverterVerticle(PRIVATE_ADDRESS, PRIVATE_PATH, ip, messages, tasks));
		vertx.deployVerticle(new ClassifierVerticle(PRIVATE_ADDRESS, PRIVATE_PATH, ip, messages, tasks));
		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			roundrobin.register((String) message.body());
		});
		vertx.setPeriodic(REGISTER_PERIODICITY, h -> {
			// System.out.println("Periodic publish : "+PRIVATE_ADDRESS+" " +
			// Thread.currentThread());
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});
		vertx.setPeriodic(ROUNDROBIN_PERIODICITY, h -> {
			for (JsonObject json : new ArrayList<>(messages)) {
				String robin = roundrobin.getNextAddress();
				if (robin != null) {
					System.out.println("Periodic round robin from " + PRIVATE_ADDRESS + " to " + robin + " " + Thread.currentThread());
					vertx.eventBus().send(robin + ":" + json.getJsonObject("task").getString(TYPE), json.encodePrettily(), TIMEOUT, replyHandler -> {
						if (replyHandler.failed())
							throw new IllegalStateException(replyHandler.cause());
						System.out.println("Receive response : " + replyHandler.result().body() + " " + Thread.currentThread());
						if (OK.equals(replyHandler.result().body()))
							messages.remove(json);
					});
				}
			}
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
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});
				vertx.deployVerticle(new DistributedVerticle(ip), result -> {
					if (complete.failed())
						throw new IllegalStateException(complete.cause());
				});

			});

		});
	}

}
