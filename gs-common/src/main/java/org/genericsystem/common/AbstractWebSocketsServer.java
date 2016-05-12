package org.genericsystem.common;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
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

	public abstract Handler<Buffer> getHandler(String path, ServerWebSocket socket);

	public abstract Handler<Buffer> getHttpHandler(HttpServerRequest request);

	public void start() {
		System.out.println("Generic System Server is starting...!");
		Vertx vertx = GSVertx.vertx().getVertx();

		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			// SLE
			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
			httpServer.requestHandler(request -> getHttpHandler(request));

			// request -> {
			//
			// String[] items = request.path().split("/");
			// if ((items.length > 1) && ("resources".equals(items[1]))) {
			// request.response().sendFile(Paths.get("").toAbsolutePath().toString() + request.path());
			// } else {
			// String indexHtml = "<!DOCTYPE html>";
			// indexHtml += "<html>";
			// indexHtml += "<head>";
			// indexHtml += "<meta charset=\"UTF-8\">";
			// indexHtml += "<script src=\"http://code.jquery.com/jquery-2.2.0.min.js\"></script>";
			// indexHtml += "<LINK rel=stylesheet type=\"text/css\" href=\"resources/style.css\"/>";
			// indexHtml += "<script>";
			// indexHtml += "var serviceLocation =\"" + url + request.path() + "\";";
			// indexHtml += "</script>";
			// indexHtml += "<script type=\"text/javascript\" src=\"resources/script.js\"></script>";
			// indexHtml += "</head>";
			// indexHtml += "<body id=\"root\">";
			// indexHtml += "</body>";
			// indexHtml += "</html>";
			// request.response().end(indexHtml);
			// }
			// });

			httpServer.websocketHandler(webSocket -> {
				String path = webSocket.path();
				System.out.println("--- socket path: " + path);
				webSocket.handler(getHandler(path, webSocket));
				webSocket.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});
			});
			AbstractBackEnd.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
		System.out.println("Generic System Server is ready!");
	}

	public void stop(Map<String, AbstractRoot> roots) {
		System.out.println("Generic System Server is stopping...");
		httpServers.forEach(httpServer -> AbstractBackEnd.<Void> synchronizeTask(handler -> httpServer.close(handler)));
		roots.values().forEach(root -> root.close());
		roots = null;
		System.out.println("Generic System Server is stopped");
	}
}
