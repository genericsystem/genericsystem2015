package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.genericsystem.kernel.Statics;

class WebSocketClient {
	private final HttpClient httpClient;
	private CompletableFuture<WebSocket> webSocketPromise = new CompletableFuture<>();

	public WebSocketClient(AbstractGSClient client, String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		webSocketPromise = getOpenPromise(client, path);
	}

	protected void open(AbstractGSClient client, String path) {
		try {
			getOpenPromise(client, path).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e1) {
			throw new IllegalThreadStateException();
		}
		;

	}

	protected <T> void send(Buffer buffer) {
		webSocketPromise.thenAccept(webSocket -> webSocket.writeBinaryMessage(buffer));
	}

	protected <T> CompletableFuture<WebSocket> getOpenPromise(AbstractGSClient client, String path) {
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
