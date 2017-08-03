package org.genericsystem.common;

import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractBackEnd {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected Map<String, Root> roots = new HashMap<>();
	protected AbstractWebSocketsServer webSocketsServer;

	public AbstractBackEnd(String host, int port) {
		webSocketsServer = buildWebSocketsServer(host, port);
	}

	abstract protected AbstractWebSocketsServer buildWebSocketsServer(String host, int port);

	public void start() {
		webSocketsServer.start();
	}

	public void stop() {
		webSocketsServer.stop(roots);
	}

	public AbstractWebSocketsServer getwebSocket() {
		return this.webSocketsServer;
	}

	public Map<String, Root> getRoots() {
		return roots;
	}

	public static <T> T synchronizeTask(Handler<Handler<AsyncResult<T>>> consumer) {
		BlockingQueue<AsyncResult<T>> blockingQueue = new ArrayBlockingQueue<>(1);
		consumer.handle(res -> {
			try {
				blockingQueue.put(res);
			} catch (InterruptedException e) {
				logger.warn("Interrupted exception.", e);
			}
		});
		AsyncResult<T> res = null;
		try {
			res = blockingQueue.take();
		} catch (InterruptedException e) {
			logger.warn("Interrupted exception.", e);
		}
		if (res.failed())
			throw new IllegalStateException(res.cause());
		return res.result();
	}

}
