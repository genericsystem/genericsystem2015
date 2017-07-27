package org.genericsystem.watch.gamma;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeoutException;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;

public abstract class ActionVerticle extends AbstractVerticle {

	private final String ip;

	public ActionVerticle(String ip) {
		this.ip = ip;
	}

	public String getIp() {
		return ip;
	}

	public String getAction() {
		throw new IllegalStateException("The getAction method must be overridden by extending classes.");
	}

	@Override
	public void start() throws Exception {
		// TODO: Abort if send timed out.
		vertx.eventBus().consumer(getAction(), message -> {
			JsonObject task = new JsonObject((String) message.body());

			if (DistributedVerticle.getMaxExecutions() <= DistributedVerticle.getExecutionsCount()) {
				message.reply(Dispatcher.KO);
				return;
			}
			DistributedVerticle.incrementExecutions();
			message.reply(Dispatcher.OK);

			vertx.executeBlocking(future -> {
				download(future, task);
				if (!future.failed())
					handle(future, task);
				else
					throw new IllegalStateException("Impossible to download file " + task.getString(DistributedVerticle.FILENAME), future.cause());
			}, res -> {
				if (res.failed() && res.cause() instanceof TimeoutException)
					return;
				handleResult(res, task);
				if (res.succeeded())
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.FINISHED).encodePrettily());
				else {
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.ABORTED).encodePrettily());
					System.out.println("Task aborted, cause: " + res.cause().getMessage());
				}
				DistributedVerticle.decrementExecutions();
			});
		});
	}

	protected abstract void handle(Future<Object> future, JsonObject task);

	protected abstract void handleResult(AsyncResult<Object> res, JsonObject task);

	private void download(Future<Object> future, JsonObject task) {
		String fileName = task.getString(DistributedVerticle.FILENAME);
		File file = new File(DistributedVerticle.BASE_PATH + fileName);
		if (!file.exists()) {
			BlockingQueue<byte[]> blockingQueue = new ArrayBlockingQueue<>(1);
			HttpClient httpClient = vertx.createHttpClient().getNow(8084, task.getString(DistributedVerticle.IP), fileName, resp -> resp.bodyHandler(body -> {
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
				file.getParentFile().mkdirs();
				fos = new FileOutputStream(file);
				fos.write(bytes);
				fos.close();
			} catch (IOException e) {
				e.printStackTrace();
				future.fail(e);
				return;
			}
		} else {
			System.out.println("File : " + fileName + " is already dowloaded");
		}
	}

	public void addTask(String fileName, String ip, String type) {
		JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO)
				.put(DistributedVerticle.IP, ip)
				.put(DistributedVerticle.FILENAME, fileName)
				.put(DistributedVerticle.TYPE, type);
		vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
	}
}
