package org.genericsystem.ir;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;

public abstract class ActionVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String ip = LocalNet.getIpAddress();

	public String getAction() {
		throw new IllegalStateException("The getAction method must be overridden by extending classes.");
	}

	@Override
	public void start() throws Exception {
		vertx.eventBus().consumer(getAction(), message -> {
			JsonObject task = new JsonObject((String) message.body());

			if (DistributedVerticle.getMaxExecutions() <= DistributedVerticle.getExecutionsCount()) {
				message.fail(1, "Too many ongoing executions on " + ip);
				return;
			}
			DistributedVerticle.incrementExecutions();
			message.reply(null);

			vertx.executeBlocking(getExecuteBlockingHandler(task), res -> {
				handleResult(res, new JsonObject(task.encode()));
				if (res.succeeded())
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.FINISHED));
				else {
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.ABORTED));
					logger.error("Task {} aborted.", task, res.cause());
				}
				DistributedVerticle.decrementExecutions();
			});
		});
	}

	protected Handler<Future<Object>> getExecuteBlockingHandler(JsonObject task) {
		return future -> {
			download(future, task);
			if (!future.failed())
				handle(future, new JsonObject(task.encode()));
		};
	}

	protected abstract void handle(Future<Object> future, JsonObject task);

	protected abstract void handleResult(AsyncResult<Object> res, JsonObject task);

	private void download(Future<Object> future, JsonObject task) {
		String fileName = task.getString(DistributedVerticle.FILENAME);
		File file = new File(DistributedVerticle.BASE_PATH + fileName);
		if (!file.exists()) {
			logger.info("Downloading file: {}", fileName);
			logger.debug("Requesting {}:8084/{}", task.getString(DistributedVerticle.IP), fileName);
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
				future.fail(e);
				return;
			}

			try (FileOutputStream fos = new FileOutputStream(file)) {
				file.getParentFile().mkdirs();
				fos.write(bytes);
			} catch (IOException e) {
				future.fail(e);
				return;
			}
			logger.debug("{} successfully downloaded ({}) ", file.getName(), String.format("%,d", file.length()));
		} else {
			logger.info("The file {} has already been downloaded.", fileName);
		}
	}

	public void addTask(String fileName, String type) {
		JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO).put(DistributedVerticle.IP, ip).put(DistributedVerticle.FILENAME, fileName).put(DistributedVerticle.TYPE, type);
		vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
	}

	public void addTask(String fileName, JsonObject jsonObject, String type) {
		JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO).put(DistributedVerticle.IP, ip).put(DistributedVerticle.FILENAME, fileName).put(DistributedVerticle.JSON_OBJECT, jsonObject).put(DistributedVerticle.TYPE, type);
		vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
	}
}
