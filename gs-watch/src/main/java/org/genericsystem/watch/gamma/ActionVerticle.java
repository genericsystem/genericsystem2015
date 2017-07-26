package org.genericsystem.watch.gamma;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
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
			JsonObject task = new JsonObject((String) handler.body());
			handler.reply(DistributedVerticle.OK);
			vertx.executeBlocking(future -> handle(future, task), res -> {
				handleResult(res, task);
				if (res.succeeded())
					task.put(Dispatcher.STATE, Dispatcher.FINISHED);
				else {
					System.out.println("Task aborted, cause: " + res.cause().getMessage());
					task.put(Dispatcher.STATE, Dispatcher.ABORTED);
				}
				vertx.eventBus().send(Dispatcher.ADDRESS + ":add", task.encodePrettily());
			});
		});
	}

	protected abstract void handle(Future<Object> future, JsonObject task);

	protected abstract void handleResult(AsyncResult<Object> res, JsonObject task);


	public void addTask(String fileName, String ip, String type) {
		JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO)
				.put(DistributedVerticle.IP, ip)
				.put(DistributedVerticle.FILENAME, fileName)
				.put(DistributedVerticle.TYPE, type);
		vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
	}
}
