package org.genericsystem.watch.beta;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

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
	private static final long MESSAGE_SEND_PERIODICITY = 5000;

	protected static final String TODO = "TODO";
	private static final String INPROGRESS = "IN PROGRESS";

	private static final String STARTED = "started";
	private static final String FINISHED = "finished";
	private static final String ABORTED = "aborted";

	private static final String OK = "OK";
	private static final String KO = "KO";

	private static final String AbsoluteAddress = System.getenv("HOME") + "/git/genericsystem2015";
	private static final String pdfDir = AbsoluteAddress + "/gs-cv/pdf";
	private static final String pngDir = AbsoluteAddress + "/gs-cv/png";
	private static final String classesDir = AbsoluteAddress + "/gs-cv/classes";

	private static final int port = 8084;

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

		System.out.println("start verticle");

		cache.safeConsum(nothing -> {
			for (Generic task1 : taskType.getInstances()) {
				if (STARTED.equals(new JsonObject((String) task1.getValue()).getString("state"))) {

					addMessage(Paths.get(new JsonObject((String) task1.getValue()).getString("file")),
							new JsonObject((String) task1.getValue()).getInteger("step"),
							new JsonObject((String) task1.getValue()).getLong("task"), TODO,
							new JsonObject((String) task1.getValue()).getInteger("max_parallel_executions"));

					task1.remove();
				}
			}

			for (Generic message1 : messageType.getInstances()) {
				if (INPROGRESS.equals(new JsonObject((String) message1.getValue()).getString("state"))) {

					addMessage(Paths.get(new JsonObject((String) message1.getValue()).getString("file")),
							new JsonObject((String) message1.getValue()).getInteger("step"),
							new JsonObject((String) message1.getValue()).getLong("task"), TODO,
							new JsonObject((String) message1.getValue()).getInteger("max_parallel_executions"));

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
							Generic inProgress = messageType.addInstance(
									new JsonObject().put("task", json.getLong("task")).put("state", INPROGRESS)
											.put("max_parallel_executions", json.getInteger("max_parallel_executions"))
											.put("step", json.getLong("step")).put("file", json.getString("file"))
											.put("IP", IP_ADDRESS).encodePrettily());
							cache.flush();
							vertx.eventBus().send(workerAddress, inProgress.getValue(), TIMEOUT, reply -> {

								cache.safeConsum(nothing -> {
									inProgress.remove();
									JsonObject js = new JsonObject((String) inProgress.getValue());
									if (reply.failed()) {
										System.out.println(reply.cause());
										roundrobin.remove(workerAddress);
										addMessage(Paths.get(js.getString("file")), js.getInteger("step"),
												js.getLong("task"), TODO, js.getInteger("max_parallel_executions"));

									} else {
										if (KO.equals(reply.result().body()))
											addMessage(Paths.get(js.getString("file")), js.getInteger("step"),
													js.getLong("task"), TODO, js.getInteger("max_parallel_executions"));
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
				message.reply(OK);

				vertx.executeBlocking(future -> {

					// get the file to convert from sender and store it in the
					// corresponding folder
					String fileType = task.getString("file").substring(task.getString("file").length() - 3);
					String remoteDirectory;
					if ("pdf".equals(fileType)) {
						remoteDirectory = pdfDir + "/";
					} else if ("png".equals(fileType) && task.getString("file").contains("classes")) {
						remoteDirectory = "";
					} else {
						remoteDirectory = pngDir + "/";
					}
					boolean success = true;

					BlockingQueue<byte[]> blockingQueue = new ArrayBlockingQueue<>(1);
					vertx.createHttpClient().getNow(port, ip_sender, remoteDirectory + task.getString("file"),
							resp -> resp.bodyHandler(body -> {
								try {
									blockingQueue.put(body.getBytes());
								} catch (InterruptedException e1) {
									e1.printStackTrace();
								}
							}));
					byte[] bytes;
					try {
						bytes = blockingQueue.take();
					} catch (InterruptedException e) {
						e.printStackTrace();
						return;
					}
					try {
						FileOutputStream fos;
						if ("png".equals(fileType)) {
							if (task.getString("file").contains("classes")) {
								fos = new FileOutputStream(new File(task.getString("file")));
								fos.write(bytes);
								fos.close();
							} else {
								fos = new FileOutputStream(new File(pngDir + "/" + task.getString("file")));
								fos.write(bytes);
								fos.close();
							}
						} else if ("pdf".equals(fileType)) {

							fos = new FileOutputStream(new File(pdfDir + "/" + task.getString("file")));
							fos.write(bytes);
							fos.close();
						}
					} catch (IOException e) {
						e.printStackTrace();
						return;
					}

					switch (task.getInteger("step")) {

					case 1:

						System.out.println("Converting pdf to png");
						convertPdfToPng(task);
						break;

					case 2:

						System.out.println("Classification");
						success = classify(task);
						break;

					case 3:

						System.out.println("OCR");
						ocr(task.getString("file"));
						break;

					}

					if (success == false) {
						future.fail("Impossible to classify image " + task.getString("file"));
					} else {
						future.complete();
					}

				}, false, res -> {
					cache.safeConsum(nothing2 -> {
						taskType.getInstance(messageTask).remove();
						if (res.succeeded()) {
							taskType.addInstance(new JsonObject().put("task", task.getLong("task"))
									.put("state", FINISHED).put("step", task.getInteger("step"))
									.put("file", task.getString("file")).encodePrettily());

						} else {
							taskType.addInstance(new JsonObject().put("task", task.getLong("task"))
									.put("state", ABORTED).put("step", task.getInteger("step"))
									.put("file", task.getString("file")).encodePrettily());
							System.out.println(res.cause());
						}
						cache.flush();
					});

				});

			});

		});

	}

	// public static <T> T synchronizeTask(Handler<Handler<AsyncResult<T>>>
	// consumer) {
	// BlockingQueue<AsyncResult<T>> blockingQueue = new
	// ArrayBlockingQueue<>(1);
	// consumer.handle(res -> {
	// try {
	// blockingQueue.put(res);
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// });
	// AsyncResult<T> res = null;
	// try {
	// res = blockingQueue.take();
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// if (res.failed())
	// throw new IllegalStateException(res.cause());
	// return res.result();
	// }

	private void ocr(String string) {

		System.out.println(">>>>> New image to OCR: " + string);
		Ocr.ocrClassifiedImage(Paths.get(string));
	}

	private boolean classify(JsonObject task) {
		Path newFile = Paths.get(task.getString("file"));
		System.out.println(">>> New file to classify: " + newFile);
		Path classesDirectory = Paths.get(classesDir);
		classesDirectory.toFile().mkdirs();
		Path savedFile;
		synchronized (ClassifierVerticle.class) {
			savedFile = Classifier.classify(classesDirectory, Paths.get(pngDir + "/" + newFile));
		}
		if (savedFile != null) {
			System.gc();
			System.runFinalization();
			cache.safeConsum(nothing -> {
				addMessage(savedFile, task.getInteger("step") + 1, task.getLong("task"), TODO,
						task.getInteger("max_parallel_executions"));
			});

			return true;

		} else
			System.out.println("Impossible to classify image " + newFile);
		return false;
	}

	private void convertPdfToPng(JsonObject task) {

		Path newFile = Paths.get(pdfDir + "/" + task.getString("file"));
		System.out.println(">> New PDF file: " + newFile);
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File(pngDir));
		for (Path path : createdPngs)
			cache.safeConsum(nothing -> {
				addMessage(path.getFileName(), task.getInteger("step") + 1, task.getLong("task"), TODO, 1);
			});

		System.gc();
		System.runFinalization();
	}

	protected void addMessage(Path file, int step, long timestamp, String state, int parallel_executions) {

		messageType.addInstance(new JsonObject().put("task", timestamp).put("state", state)
				.put("max_parallel_executions", parallel_executions).put("step", step).put("file", file.toString())
				.encodePrettily());
		cache.flush();

	}

	protected void startServer() {

		vertx.createHttpServer().requestHandler(req -> {
			// String fileName = req.path().replace("/", "");
			String fileName = req.path();
			System.out.println("Will send :" + fileName);
			req.response().sendFile(fileName);
		}).listen(port);
	}
}
