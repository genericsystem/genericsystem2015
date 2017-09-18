package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;

import org.genericsystem.common.Generic;
import org.genericsystem.ir.Model.Task;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonObject;

public class Dispatcher extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	protected final Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/tasks/", Task.class);
	protected Cache cache = engine.newCache();
	protected final Generic taskType = engine.find(Task.class);

	private static final String EMAIL_SETTINGS = DistributedVerticle.BASE_PATH + "/.conf/MailWatcherVerticle.json";

	public static final String ADDRESS = "org.genericsystem.repartitor";
	protected static final String TASK = "task";
	protected static final String NEW_STATE = "newState";
	protected static final String STATE = "state";
	protected static final String TODO = "todo";
	protected static final String RUNNING = "running";
	protected static final String FINISHED = "finished";
	protected static final String ABORTED = "aborted";
	private static final long MESSAGE_SEND_PERIODICITY = 5000;

	public static void main(String[] args) {
		Dispatcher dispatcher = new Dispatcher();
		dispatcher.doDeploy();
	}

	public void doDeploy() {
		Tools.deployOnCluster(vertx -> {
			vertx.deployVerticle(new HttpServerVerticle(), complete -> {
				if (complete.failed())
					throw new IllegalStateException(complete.cause());
			});
			vertx.deployVerticle(this, res -> {
				if (res.failed())
					throw new IllegalStateException(res.cause());
			});
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		cache.safeConsum(nothing -> {
			for (Generic task : taskType.getInstances()) {
				JsonObject json = new JsonObject((String) task.getValue());
				if (RUNNING.equals(json.getString(STATE)))
					updateTaskState(json, TODO);
			}
		});
		watchMail();
		vertx.deployVerticle(new HttpServerVerticle(), ar -> {
			if (ar.failed())
				throw new IllegalStateException("Unable to create HTTP server.", ar.cause());
			else
				logger.info("HTTP server started.");
		});
		vertx.eventBus().consumer(ADDRESS + ":watchMail", message -> {
			logger.info("Restarting mail watcher thread…");
			watchMail();
		});
		vertx.eventBus().consumer(ADDRESS + ":updateState", message -> {
			JsonObject json = (JsonObject) message.body();
			updateTaskState(json.getJsonObject(TASK), json.getString(NEW_STATE));
		});
		vertx.eventBus().consumer(ADDRESS + ":add", message -> {
			cache.safeConsum(unused -> {
				taskType.addInstance((String) message.body());
				cache.flush();
			});
		});
		vertx.setPeriodic(MESSAGE_SEND_PERIODICITY, h -> {
			cache.safeConsum(unused -> {
				for (Generic task : taskType.getInstances()) {
					JsonObject json = new JsonObject((String) task.getValue());
					if (TODO.equals(json.getString(STATE))) {
						vertx.eventBus().send(json.getString(DistributedVerticle.TYPE), new JsonObject(json.encode()).put(STATE, RUNNING).encodePrettily(), reply -> {
							if (reply.failed())
								switch (((ReplyException) reply.cause()).failureType()) {
								case NO_HANDLERS:
									logger.warn("No handler for task: {}.", json.encodePrettily());
									break;
								case TIMEOUT:
									logger.warn("Sending of task {} timed out.", reply.cause(), json.encodePrettily());
									break;
								case RECIPIENT_FAILURE:
									logger.info("Task {} rejected by recipient.", reply.cause(), json.encodePrettily());
									break;
								}
							else
								updateTaskState(json, RUNNING);
						});
					}
				}
				cache.flush();
			});
		});
		startFuture.complete();
	}

	private void updateTaskState(JsonObject oldValue, String newState) {
		logger.debug("Updating: {}, newState: {}.", oldValue.encodePrettily(), newState);
		cache.safeConsum(unused -> {
			Generic task = taskType.getInstances().filter(g -> oldValue.equals(new JsonObject((String) g.getValue()))).first();
			JsonObject newValue = new JsonObject(oldValue.encode()).put(STATE, newState);
			task.update(newValue.encodePrettily());
			cache.flush();
		});
	}

	private void watchMail() {
		vertx.fileSystem().readFile(EMAIL_SETTINGS, ar -> {
			if (ar.failed())
				throw new IllegalStateException("Impossible to load configuration for MailWatcherVerticle.", ar.cause());
			else {
				JsonObject config = new JsonObject(ar.result());
				vertx.deployVerticle(new MailWatcherVerticle(), new DeploymentOptions().setConfig(config), res -> {
					if (res.failed())
						throw new IllegalStateException("Unable to deploy MailWatcherVerticle", res.cause());
				});
			}
		});
	}
}
