package org.genericsystem.remote;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import java.util.concurrent.CompletableFuture;

import org.genericsystem.common.GSVertx;

/**
 * @author Nicolas Feybesse
 *
 */
class WebSocketClient {
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
			System.out.println("Close socket");
		} catch (Exception ignore) {
		}
		try {
			httpClient.close();
			System.out.println("Close httpClient");
		} catch (Exception ignore) {
		}
	}

}
