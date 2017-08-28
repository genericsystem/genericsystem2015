package org.genericsystem.remote;

import java.lang.invoke.MethodHandles;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.common.GSVertx;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
class WebSocketClient {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private final HttpClient httpClient;
	private CompletableFuture<WebSocket> webSocketPromise = new CompletableFuture<>();

	public WebSocketClient(FrontEnd client, String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		webSocketPromise = getOpenPromise(client, path);
	}

	protected <T> void send(Buffer buffer) {
		webSocketPromise.thenAccept(webSocket -> webSocket.writeBinaryMessage(buffer));
	}

	protected <T> CompletableFuture<WebSocket> getOpenPromise(FrontEnd client, String path) {
		CompletableFuture<WebSocket> promise = new CompletableFuture<>();
		httpClient.websocket(path, webSock -> {
			webSock.exceptionHandler(e -> {
				promise.completeExceptionally(e);
			});
			webSock.handler(client.getHandler());
			promise.complete(webSock);
		});
		return promise;
	}

	public void close() {
		try {
			webSocketPromise.thenAccept(webSocket -> webSocket.close());
			logger.info("Close socket");
		} catch (Exception ignore) {
		}
		try {
			httpClient.close();
			logger.info("Close httpClient");
		} catch (Exception ignore) {
		}
	}

}
