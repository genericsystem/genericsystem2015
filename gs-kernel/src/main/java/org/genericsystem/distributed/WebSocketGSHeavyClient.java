package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

public class WebSocketGSHeavyClient extends AbstractGSHeavyClient {

	private final HttpClient httpClient;
	private final WebSocket webSocket;

	WebSocketGSHeavyClient(String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		webSocket = synchonizeTask(task -> httpClient.websocket(path, task));
		webSocket.exceptionHandler(e -> {
			System.out.println("Discard http request because of : ");
			e.printStackTrace();
		});
	}

	// TODO synchronize this method ?
	@Override
	<T> void send(Buffer buffer, Handler<Buffer> responseHandler) {
		webSocket.handler(responseHandler);
		webSocket.writeBinaryMessage(buffer);
	}

	@Override
	public void close() {
		try {
			webSocket.close();
			System.out.println("Close socket");
		} catch (Exception ignore) {}
		try {
			httpClient.close();
			System.out.println("Close httpClient");
		} catch (Exception ignore) {}
	}
}
