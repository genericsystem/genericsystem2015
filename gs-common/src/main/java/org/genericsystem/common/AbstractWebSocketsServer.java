package org.genericsystem.common;

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
public abstract class AbstractWebSocketsServer {
	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;
	private final String url;

	public AbstractWebSocketsServer(String host, int port) {
		this.port = port;
		this.host = host;
		this.url = "ws://" + host + ":" + port;
		System.out.println("url: " + this.url);
	}

	public String getUrl() {
		return this.url;
	}

	public abstract Handler<Buffer> getHandler(String path, ServerWebSocket socket);

	// public abstract Handler<Buffer> getHttpHandler(String path, HttpServerRequest request, String url);

	public void start() {
		System.out.println("Generic System Server is starting...!");
		Vertx vertx = GSVertx.vertx().getVertx();

		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			// SLE

			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));

			httpServer.websocketHandler(webSocket -> {
				String path = webSocket.path();
				webSocket.handler(getHandler(path, webSocket));
				webSocket.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});
			});

			addHttpHandler(httpServer, getUrl());

			AbstractBackEnd.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);

		}
		System.out.println("Generic System Server is ready!");
	}

	public abstract void addHttpHandler(HttpServer httpServer, String url);

	public void stop(Map<String, AbstractRoot> roots) {
		System.out.println("Generic System Server is stopping...");
		httpServers.forEach(httpServer -> AbstractBackEnd.<Void> synchronizeTask(handler -> httpServer.close(handler)));
		roots.values().forEach(root -> root.close());
		roots = null;
		System.out.println("Generic System Server is stopped");
	}

}
