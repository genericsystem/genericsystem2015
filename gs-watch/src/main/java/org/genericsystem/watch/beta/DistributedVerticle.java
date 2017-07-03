package org.genericsystem.watch.beta;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.PdfToPngConverter;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;
import org.genericsystem.watch.ClassifierVerticle;
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

	protected final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/cloud/", Task.class,
			Message.class);
	protected final Cache cache = engine.newCache();
	protected final Generic messageType = engine.find(Message.class);
	protected final Generic taskType = engine.find(Task.class);
	private final RoundRobin roundrobin = new RoundRobin();
	private static final DeliveryOptions TIMEOUT = new DeliveryOptions().setSendTimeout(2000);

	private static final String IP_ADDRESS = "192.168.1.11";
	private static final int ATTEMPTS = 5;

	private static final long AVAILABILITY_PERIODICITY = 5000;
	private static final long TASK_CREATION_PERIODICITY = 10000;
	private static final long MESSAGE_SEND_PERIODICITY = 1000;

	protected static final String TODO = "TODO";
	private static final String INPROGRESS = "IN PROGRESS";

	private static final String STARTED = "started";
	private static final String FINISHED = "finished";
	private static final String ABORTED = "aborted";

	private static final String OK = "OK";
	private static final String KO = "KO";

	private static final String pdfDir = "../src/main/resources/pdf";
	private static final String pngDir = "../src/main/resources/png";

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
					messageType.addInstance(new JsonObject()
							.put("task", new JsonObject((String) task1.getValue()).getLong("task")).put("state", TODO)
							.put("max_parallel_executions", 5)
							.put("step", new JsonObject((String) task1.getValue()).getInteger("step"))
							.put("file", new JsonObject((String) task1.getValue()).getString("file")).encodePrettily());
					task1.remove();
				}
			}

			for (Generic message1 : messageType.getInstances()) {
				if (INPROGRESS.equals(new JsonObject((String) message1.getValue()).getString("state"))) {
					messageType.addInstance(
							new JsonObject().put("task", new JsonObject((String) message1.getValue()).getLong("task"))
									.put("state", TODO).put("max_parallel_executions", 5)
									.put("step", new JsonObject((String) message1.getValue()).getInteger("step"))
									.put("file", new JsonObject((String) message1.getValue()).getString("file"))
									.encodePrettily());
					message1.remove();
				}
			}
			cache.flush();
		});

		startServer();

		// Periodic aknowledge of availability
		vertx.setPeriodic(AVAILABILITY_PERIODICITY, h -> {
			vertx.eventBus().publish(PUBLIC_ADDRESS, PRIVATE_ADDRESS);
		});

		vertx.eventBus().consumer(PUBLIC_ADDRESS, message -> {
			roundrobin.Register((String) message.body());
		});

		// // Periodic : Task creation
		// vertx.setPeriodic(TASK_CREATION_PERIODICITY, m -> {
		// cache.safeConsum(n -> {
		// messageType.addInstance(new JsonObject().put("task",
		// System.currentTimeMillis()).put("state", TODO)
		// .put("max_parallel_executions", 5).put("step", 1).put("file",
		// "/image-0.png").encodePrettily());
		// cache.flush();
		// });
		// });

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
							Generic inProgress = messageType.addInstance(new JsonObject()
									.put("task", json.getLong("task")).put("state", INPROGRESS)
									.put("max_parallel_executions", 5).put("step", json.getLong("step"))
									.put("file", json.getString("file")).put("IP", IP_ADDRESS).encodePrettily());
							cache.flush();
							vertx.eventBus().send(workerAddress, inProgress.getValue(), TIMEOUT, reply -> {
								cache.safeConsum(nothing -> {
									inProgress.remove();
									JsonObject js = new JsonObject((String) inProgress.getValue());
									if (reply.failed()) {
										System.out.println(reply.cause());
										roundrobin.remove(workerAddress);
										messageType.addInstance(new JsonObject().put("task", js.getLong("task"))
												.put("state", TODO).put("max_parallel_executions", 5)
												.put("step", js.getInteger("step")).put("file", js.getString("file"))
												.encodePrettily());
									} else {
										if (KO.equals(reply.result().body()))
											messageType.addInstance(new JsonObject().put("task", js.getLong("task"))
													.put("state", TODO).put("max_parallel_executions", 5)
													.put("step", js.getInteger("step"))
													.put("file", js.getString("file")).encodePrettily());
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

				JsonObject task = new JsonObject((String) message.body());
				String ip_sender = task.getString("IP");
				String messageTask = new JsonObject().put("task", task.getLong("task")).put("state", STARTED)
						.put("step", task.getInteger("step")).put("file", task.getString("file")).encodePrettily();
				taskType.addInstance(messageTask);
				cache.flush();
				vertx.executeBlocking(future -> {

					// get the file to convert from sender and store it in the
					// corresponding folder
					String fileType = task.getString("file").substring(task.getString("file").length() - 3);
					boolean success = true;
					vertx.createHttpClient().getNow(8080, ip_sender, task.getString("file"), resp -> {

						resp.bodyHandler(body -> {
							FileOutputStream fos;
							try {

								if (fileType == "png") {
									fos = new FileOutputStream(new File(pngDir + "/" + task.getString("file")));
									fos.write(body.getBytes());
									fos.close();
								} else if (fileType == "pdf") {
									fos = new FileOutputStream(new File(pdfDir + "/" + task.getString("file")));
									fos.write(body.getBytes());
									fos.close();
								}

							} catch (Exception e) {
								e.printStackTrace();
							}

						});
					});

					switch (task.getInteger("step")) {

					case 1:

						ConvertPdfToPng(task.getString("file"));

						break;

					case 2:

						success = Classify(task.getString("file"));

						break;

					case 3:

						Ocr(task.getString("file"));
						break;

					}

					if (success == false) {
						future.fail("Impossible to classify image " + task.getString("file"));
					} else {
						future.complete();
					}

				}, res -> {
					cache.safeConsum(nothing2 -> {
						taskType.getInstance(messageTask).remove();
						if (res.succeeded()) {
							taskType.addInstance(new JsonObject().put("task", task.getLong("task"))
									.put("state", FINISHED).put("step", task.getInteger("step"))
									.put("file", task.getString("file")).encodePrettily());

						} else
							taskType.addInstance(new JsonObject().put("task", task.getLong("task"))
									.put("state", ABORTED).put("step", task.getInteger("step"))
									.put("file", task.getString("file")).encodePrettily());
						cache.flush();
					});

				});
				message.reply(OK);
			});

		});

	}

	private void Ocr(String string) {

		String imagePath = string;
		System.out.println(">>>>> New image to OCR: " + imagePath);
		Ocr.ocrClassifiedImage(Paths.get(imagePath));
	}

	private boolean Classify(String filename) {

		Path newFile = Paths.get(filename);
		System.out.println(">>> New file to classify: " + newFile);
		Path classesDirectory = Paths.get("classes");
		classesDirectory.toFile().mkdirs();
		Path savedFile;
		synchronized (ClassifierVerticle.class) {
			savedFile = Classifier.classify(classesDirectory, newFile);
		}
		if (savedFile != null) {
			System.gc();
			System.runFinalization();
			cache.safeConsum(n -> {
				messageType.addInstance(new JsonObject().put("task", System.currentTimeMillis()).put("state", TODO)
						.put("max_parallel_executions", 5).put("step", 3).put("file", savedFile).encodePrettily());
				cache.flush();
			});
			return true;

		} else
			System.out.println("Impossible to classify image " + newFile);
		return false;
	}

	private void ConvertPdfToPng(String filename) {

		Path newFile = Paths.get(filename);
		System.out.println(">> New PDF file: " + newFile);
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File(pngDir));
		for (Path path : createdPngs)
			// publish message
			cache.safeConsum(n -> {
				messageType.addInstance(new JsonObject().put("task", System.currentTimeMillis()).put("state", TODO)
						.put("max_parallel_executions", 5).put("step", 2).put("file", path).encodePrettily());
				cache.flush();
			});
		System.gc();
		System.runFinalization();
	}

	public void startServer() {

		vertx.createHttpServer().requestHandler(req -> {
			String fileName = req.path().replace("/", "");
			System.out.println("Will send :" + fileName);
			req.response().sendFile(fileName);
		}).listen(8080);
	}
}
