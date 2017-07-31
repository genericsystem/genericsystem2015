package org.genericsystem.watch;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;

public abstract class ActionVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(ActionVerticle.class);
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

			vertx.executeBlocking(future -> {
				download(future, task);
				if (!future.failed())
					handle(future, task);
			}, res -> {
				handleResult(res, task);
				if (res.succeeded())
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.FINISHED).encodePrettily());
				else {
					vertx.eventBus().send(Dispatcher.ADDRESS + ":updateState", new JsonObject().put(Dispatcher.TASK, task).put(Dispatcher.NEW_STATE, Dispatcher.ABORTED).encodePrettily());
					logger.error("Task " + task + " aborted.", res.cause());
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
				future.fail(e);
				return;
			}
		} else {
			logger.info("The file " + fileName + " has already been downloaded.");
		}
	}

	public void addTask(String fileName, String type) {
		JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO)
				.put(DistributedVerticle.IP, ip)
				.put(DistributedVerticle.FILENAME, fileName)
				.put(DistributedVerticle.TYPE, type);
		vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
	}
}
