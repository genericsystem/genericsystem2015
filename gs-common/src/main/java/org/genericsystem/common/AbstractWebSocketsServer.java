package org.genericsystem.common;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractWebSocketsServer {
	protected static Logger log = LoggerFactory.getLogger(AbstractWebSocketsServer.class);
	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;

	public AbstractWebSocketsServer(String host, int port) {
		this.port = port;
		this.host = host;
	}

	public abstract Handler<Buffer> getHandler(String path, ServerWebSocket socket);

	public abstract void addHttpHandler(HttpServer httpServer);

	public void start() {
		log.info("Generic System Server is starting...!");
		Vertx vertx = GSVertx.vertx().getVertx();

		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			// SLE
			// cmd line : keytool -genkey -keyalg RSA -alias genericsystem -keystore keystore.jks -storepass middleware -validity 360
			// url doc : https://www.javacodegeeks.com/2014/07/java-keystore-tutorial.html
			// HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host).setSsl(true)
			// .setKeyStoreOptions(new JksOptions().setPath("keystore.jks").setPassword(new String("middleware"))));

			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));

			httpServer.websocketHandler(webSocket -> {
				String path = webSocket.path();
				webSocket.handler(getHandler(path, webSocket));
				webSocket.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});

				webSocket.closeHandler((v) -> System.out.println("close the webSocket"));
				;
			});

			addHttpHandler(httpServer);

			AbstractBackEnd.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
	}

	public void stop(Map<String, Root> roots) {
		log.info("Generic System Server is stopping...");
		httpServers.forEach(httpServer -> AbstractBackEnd.<Void> synchronizeTask(handler -> httpServer.close(handler)));
		roots.values().forEach(root -> root.close());
		roots = null;
		log.info("Generic System Server is stopped");
	}
}
