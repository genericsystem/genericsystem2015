package org.genericsystem.distributed;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractBackEnd<T extends Closable> {

	protected Map<String, T> roots = new HashMap<>();
	protected AbstractWebSocketsServer<T> webSocketsServer;

	public AbstractBackEnd(GSDeploymentOptions options) {
		webSocketsServer = buildWebSocketsServer(options);
	}

	abstract protected AbstractWebSocketsServer<T> buildWebSocketsServer(GSDeploymentOptions options);

	public void start() {
		webSocketsServer.start(roots);
	}

	public void stop() {
		webSocketsServer.stop(roots);
	}

	public AbstractWebSocketsServer<T> getwebSocket() {
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
