package org.genericsystem.watch.gamma;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;

public abstract class ActionVerticle extends AbstractVerticle {

	private final String privateAddress;
	private final String ip;

	public ActionVerticle(String privateAddress, String ip) {
		this.privateAddress = privateAddress;
		this.ip = ip;
	}

	public String getPrivateAddress() {
		return privateAddress;
	}

	public String getIp() {
		return ip;
	}

	public String getAction() {
		throw new IllegalStateException("The getAction method must be overridden by extending classes.");
	}

	@Override
	public void start() throws Exception {
		vertx.eventBus().consumer(privateAddress + ":" + getAction(), handler -> {
			System.out.println("Receive from : " + handler.body() + " on : " + privateAddress + " " + Thread.currentThread());
			handler.reply(DistributedVerticle.OK);
			JsonObject task = new JsonObject((String) handler.body());
			vertx.executeBlocking(future -> download(future, task), res -> {
				if (res.succeeded())
					vertx.executeBlocking(future_ -> handle(future_, task), res_ -> {
						handleResult(res_, task);
						if (res_.succeeded())
							task.put(Dispatcher.STATE, Dispatcher.FINISHED);
						else {
							System.out.println("Task aborted, cause: " + res_.cause().getMessage());
							task.put(Dispatcher.STATE, Dispatcher.ABORTED);
						}
						vertx.eventBus().send(Dispatcher.ADDRESS + ":add", task.encodePrettily());
					});
				else
					throw new IllegalStateException("Impossible to download file " + task.getString(DistributedVerticle.FILENAME), res.cause());
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
				future.complete(file.toString());
			} catch (IOException e) {
				e.printStackTrace();
				future.fail(e);
				return;
			}
		} else {
			System.out.println("File : " + fileName + " is already dowloaded");
			future.complete(file.toString());	
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
