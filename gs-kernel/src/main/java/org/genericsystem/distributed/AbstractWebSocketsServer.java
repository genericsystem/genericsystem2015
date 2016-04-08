package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.ServerWebSocket;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractWebSocketsServer<T extends Closable> {
	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;

	public AbstractWebSocketsServer(String host, int port) {
		this.port = port;
		this.host = host;
	}

	public abstract Handler<Buffer> getHandler(T root, ServerWebSocket socket);

	public void start(Map<String, T> roots) {
		System.out.println("Generic System Server is starting...!");
		Vertx vertx = GSVertx.vertx().getVertx();
		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
			httpServer.websocketHandler(webSocket -> {
				String path = webSocket.path();
				T root = roots.get(path);
				if (root == null)
					throw new IllegalStateException("Unable to find database :" + path);
				webSocket.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});
				webSocket.handler(getHandler(root, webSocket));
			});
			AbstractBackEnd.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
		System.out.println("Generic System Server is ready!");
	}

	public void stop(Map<String, T> roots) {
		System.out.println("Generic System Server is stopping...");
		httpServers.forEach(httpServer -> AbstractBackEnd.<Void> synchronizeTask(handler -> httpServer.close(handler)));
		roots.values().forEach(root -> root.close());
		roots = null;
		System.out.println("Generic System Server is stopped");
	}
}
