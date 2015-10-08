package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import org.genericsystem.distributed.GSVertx;

public class WebSocketGSLightClient extends AbstractGSLightClient {
	private final HttpClient httpClient;
	private final WebSocket webSocket;

	WebSocketGSLightClient(String host, int port, String path) {
		httpClient = GSVertx.vertx().getVertx().createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST));
		webSocket = synchronizeTask(task -> httpClient.websocket(path, webSock -> task.handle(webSock)));
		webSocket.exceptionHandler(e -> {
			System.out.println("Discard http request because of : ");
			e.printStackTrace();
		});
	}

	// TODO synchronize this method ?
	@Override
	protected <T> void send(Buffer buffer, Handler<Buffer> responseHandler) {
		webSocket.handler(responseHandler);
		webSocket.write(buffer);
	}

	@Override
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
