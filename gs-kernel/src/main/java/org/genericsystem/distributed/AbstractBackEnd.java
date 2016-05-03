package org.genericsystem.distributed;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.kernel.AbstractServer;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractBackEnd {

	protected Map<String, AbstractServer> roots = new HashMap<>();
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

	public static <T> T synchronizeTask(Handler<Handler<AsyncResult<T>>> consumer) {
		BlockingQueue<AsyncResult<T>> blockingQueue = new ArrayBlockingQueue<>(1);
		consumer.handle(res -> {
			try {
				blockingQueue.put(res);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		});
		AsyncResult<T> res = null;
		try {
			res = blockingQueue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		if (res.failed())
			throw new IllegalStateException(res.cause());
		return res.result();
	}

}
