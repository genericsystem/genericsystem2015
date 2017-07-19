package org.genericsystem.watch.gamma;

import java.util.List;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public abstract class ActionVerticle extends AbstractVerticle {

	private final String privateAddress;
	private final String privatePath;
	private final String ip;
	private final List<JsonObject> messages;
	private final List<JsonObject> tasks;

	public ActionVerticle(String privateAddress, String privatePath, String ip, List<JsonObject> messages, List<JsonObject> tasks) {
		this.privateAddress = privateAddress;
		this.privatePath = privatePath;
		this.ip = ip;
		this.messages = messages;
		this.tasks = tasks;
	}

	public String getPrivateAddress() {
		return privateAddress;
	}

	public String getPrivatePath() {
		return privatePath;
	}

	public String getIp() {
		return ip;
	}

	public List<JsonObject> getMessages() {
		return messages;
	}

	public List<JsonObject> getTasks() {
		return tasks;
	}

	public String getAction() {
		throw new IllegalStateException("The getAction method must be overridden by extending classes.");
	}

	@Override
	public void start() throws Exception {
		vertx.eventBus().consumer(privateAddress + ":" + getAction(), handler -> {
			System.out.println("Receive from : " + (String) handler.body() + " on : " + privateAddress + " " + Thread.currentThread());
			JsonObject task = new JsonObject((String) handler.body()).getJsonObject("task");
			tasks.add(task);
			handler.reply(DistributedVerticle.OK);
			String fileName = task.getString(DistributedVerticle.FILENAME);
			vertx.executeBlocking(future -> handle(future, fileName, task), res -> {
				handleResult(res, fileName);
				System.out.println("Blocking task callback on thread : " + Thread.currentThread());
				System.out.println("Task " + task.encodePrettily() + " is done, removing " + Thread.currentThread());
				tasks.remove(task);			
			});
		});
	}

	protected abstract void handle(Future<Object> future, String fileName, JsonObject task);

	protected abstract void handleResult(AsyncResult<Object> res, String fileName);
}
