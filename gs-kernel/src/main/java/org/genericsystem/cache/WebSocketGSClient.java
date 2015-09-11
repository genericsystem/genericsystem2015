package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

public class WebSocketGSClient extends AbstractGSClient {

	private HttpClient httpClient;
	private WebSocket webSocket;

	WebSocketGSClient(String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		webSocket = synchonizeTask(task -> httpClient.websocket(path, task));
		webSocket.exceptionHandler(e -> {
			System.out.println("Discard http request because of : ");
			e.printStackTrace();
		});
	}

	@Override
	<T> void send(Buffer buffer, Handler<Buffer> responseHandler) {
		webSocket.handler(responseHandler);
		webSocket.writeBinaryMessage(buffer);
	}

	@Override
	public void close() {
		System.out.println("Close socket");
		if (webSocket != null)
			webSocket.close();
		webSocket = null;
		System.out.println("Close httpclient");
		if (httpClient != null)
			httpClient.close();
		httpClient = null;
	}
}
