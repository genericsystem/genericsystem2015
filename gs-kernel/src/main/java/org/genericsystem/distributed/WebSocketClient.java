package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

class WebSocketClient {
	private final HttpClient httpClient;
	private final WebSocket webSocket;
	
	public WebSocketClient(AbstractGSClient client, String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		// /!\
		webSocket = AbstractGSClient.synchronizeTask(task -> httpClient.websocket(path, webSock -> task.handle(webSock)));
		webSocket.exceptionHandler(e -> {
			System.out.println("Discard http request because of : ");
			e.printStackTrace();
		});
		webSocket.handler(client.getHandler());
	}

	protected <T> void send(Buffer buffer) {
		webSocket.writeBinaryMessage(buffer);
	}

	public void close() {
		try {
			webSocket.close();
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
