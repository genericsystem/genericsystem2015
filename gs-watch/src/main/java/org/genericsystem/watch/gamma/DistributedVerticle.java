package org.genericsystem.watch.gamma;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.PdfToPngConverter;
import org.genericsystem.watch.ClassifierVerticle;
import org.genericsystem.watch.beta.RoundRobin;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class DistributedVerticle extends AbstractVerticle {
	private static final long REGISTER_PERIODICITY = 1000;
	private static final long ROUNDROBIN_PERIODICITY = 5000;
	private static final String PUBLIC_ADDRESS = "publicAddress";
	public static final String BASE_PATH = System.getenv("HOME") + "/git/genericsystem2015/gs-cv/";
	private static final String FILENAME = "filename";
	private static final String OK = "OK";
	private static final String KO = "KO";
	private static final String ID = "ID";
	private static final String TYPE = "type";
	private static final String IP = "IP";
	private final String PRIVATE_ADDRESS;
	private final String PRIVATE_PATH;
	private final String DOWNLOAD = "download";
	private final String PDF_TO_PNG = "pdfToPng";
	private final String CLASSIFY = "classification";
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
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image.pdf").put(IP, ip).put(TYPE, DOWNLOAD)));
		id = System.currentTimeMillis();
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image2.pdf").put(IP, ip).put(TYPE, DOWNLOAD)));
		id = System.currentTimeMillis();
		messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, "pdf/image3.pdf").put(IP, ip).put(TYPE, DOWNLOAD)));
	}

	@Override
	public void start() throws Exception {

		vertx.eventBus().consumer(PRIVATE_ADDRESS + ":" + DOWNLOAD, handler -> {
			System.out.println("Receive from : " + (String) handler.body() + " on : " + PRIVATE_ADDRESS + " " + Thread.currentThread());
			JsonObject task = new JsonObject((String) handler.body()).getJsonObject("task");
			tasks.add(task);
			handler.reply(OK);
			String fileName = task.getString(FILENAME);
			File file = new File(PRIVATE_PATH + fileName);
			if (!file.exists()) {
				vertx.executeBlocking(future -> download(future, fileName, task.getString(IP)), result -> {
					if (result.failed()) {
						System.out.println(result.cause());
						throw new IllegalStateException(result.cause());
					} else {
						System.out.println("Download successful " + fileName);
						long id = System.currentTimeMillis();
						messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, fileName).put(IP, ip).put(TYPE, PDF_TO_PNG)));
					}
					System.out.println("Blocking task callback on thread : " + Thread.currentThread());
					System.out.println("Task " + task.encodePrettily() + " is done, removing " + Thread.currentThread());
					tasks.remove(task);
				});
			} else
				System.out.println("File : " + fileName + " is already dowloaded");
		});
		vertx.eventBus().consumer(PRIVATE_ADDRESS + ":" + PDF_TO_PNG, handler -> {
			System.out.println("Receive from : " + (String) handler.body() + " on : " + PRIVATE_ADDRESS + " " + Thread.currentThread());
			JsonObject task = new JsonObject((String) handler.body()).getJsonObject("task");
			tasks.add(task);
			handler.reply(OK);
			String fileName = task.getString(FILENAME);
			File file = new File(PRIVATE_PATH + fileName);
			vertx.executeBlocking(future -> {
				List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(file, new File("../gs-cv/png"));
				future.complete(createdPngs);
			}, res -> {
				if (res.succeeded()) {
					for (Path newPng : (List<Path>) res.result()) {
						long id = System.currentTimeMillis();
						messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, newPng.toString()).put(IP, ip).put(TYPE, CLASSIFY)));
						System.out.println("New PNG file : " + newPng);
					}
					System.out.println("Blocking task callback on thread : " + Thread.currentThread());
					System.out.println("Task " + task.encodePrettily() + " is done, removing " + Thread.currentThread());
					tasks.remove(task);
				}
			});
		});
		vertx.eventBus().consumer(PRIVATE_ADDRESS + ":" + CLASSIFY, handler -> {
			System.out.println("Receive from : " + (String) handler.body() + " on : " + PRIVATE_ADDRESS + " " + Thread.currentThread());
			JsonObject task = new JsonObject((String) handler.body()).getJsonObject("task");
			tasks.add(task);
			handler.reply(OK);
			String fileName = task.getString(FILENAME);
			File file = new File(fileName);
			vertx.executeBlocking(future -> {
				Path savedFile;
				synchronized (ClassifierVerticle.class) {
					savedFile = Classifier.classify(Paths.get("../gs-cv/classes/"), file.toPath());
				}
				if (savedFile != null)
					future.complete(savedFile);
				else
					future.fail("Impossible to classify image.");
			}, res -> {
				if (res.succeeded()) {
					//					long id = System.currentTimeMillis();
					//						messages.add(new JsonObject().put(ID, id).put("task", new JsonObject().put(ID, id).put(FILENAME, newPng).put(IP, ip).put(TYPE, CLASSIFY)));
					System.out.println("Image classified : " + res.result());
				} else {
					System.out.println("Impossible to classify image : " + file);
				}
				System.out.println("Blocking task callback on thread : " + Thread.currentThread());
				System.out.println("Task " + task.encodePrettily() + " is done, removing " + Thread.currentThread());
				tasks.remove(task);
			});
		});
		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			System.out.println(roundrobin);
			roundrobin.register((String) message.body());
		});
		vertx.setPeriodic(REGISTER_PERIODICITY, h -> {
			// System.out.println("Periodic publish : "+PRIVATE_ADDRESS+" " +
			// Thread.currentThread());
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});
		// vertx.setPeriodic(ROUNDROBIN_PERIODICITY, h -> {
		// for (JsonObject json : new ArrayList<>(messages)) {
		// String robin = roundrobin.getNextAddress();
		// if (robin != null) {
		// System.out.println("Periodic round robin from " + PRIVATE_ADDRESS + " to " + robin + " " + Thread.currentThread());
		// vertx.eventBus().send(robin, json.encodePrettily(), TIMEOUT, replyHandler -> {
		// if (replyHandler.failed())
		// throw new IllegalStateException(replyHandler.cause());
		// System.out.println("Receive response : " + replyHandler.result().body() + " " + Thread.currentThread());
		// if (OK.equals(replyHandler.result().body()))
		// messages.remove(json);
		// });
		// }
		// }
		// });
	}

	private <T> void download(Future<T> future, String fileName, String ip) {
		// System.out.println("Execute blocking task : " +
		// Thread.currentThread());
		BlockingQueue<byte[]> blockingQueue = new ArrayBlockingQueue<>(1);
		HttpClient httpClient = vertx.createHttpClient().getNow(8084, ip, fileName, resp -> resp.bodyHandler(body -> {
			// System.out.println("get Bytes : " + Thread.currentThread());
			try {
				blockingQueue.put(body.getBytes());
			} catch (InterruptedException e) {
				future.fail(e);
				return;
			}
		}));
		httpClient.close();
		byte[] bytes;
		try {
			bytes = blockingQueue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			future.fail(e);
			return;
		}
		FileOutputStream fos;

		try {
			File file = new File(PRIVATE_PATH + fileName);
			new File(PRIVATE_PATH + fileName.substring(0, fileName.lastIndexOf("/"))).mkdirs();
			fos = new FileOutputStream(file);
			fos.write(bytes);
			fos.close();
		} catch (IOException e) {
			e.printStackTrace();
			future.fail(e);
			return;
		}
		// System.out.println("Blocking task complete on thread : " +
		// Thread.currentThread());
		future.complete();
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
