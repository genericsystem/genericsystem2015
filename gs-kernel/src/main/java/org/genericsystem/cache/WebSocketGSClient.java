package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import java.util.concurrent.CountDownLatch;

public class WebSocketGSClient extends AbstractGSClient {

	private final WebSocket webSocket;

	WebSocketGSClient(Vertx vertx, ClientEngine engine, String host, int port, String path) {
		super(engine);
		CountDownLatch cdl = new CountDownLatch(1);
		WebSocket[] socketArray = new WebSocket[1];
		vertx.createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST)).websocket(path, socket -> {
			socket.handler(getHandler());
			socketArray[0] = socket;
			cdl.countDown();
		});

		try {
			cdl.await();
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
			e.printStackTrace();
		}
		webSocket = socketArray[0];
		webSocket.exceptionHandler(e -> {
			e.printStackTrace();
		});
	}

	@Override
	void send(Buffer buffer) {
		webSocket.writeFinalBinaryFrame(buffer);
	}

	@Override
	public void close() {
		webSocket.close();
	}
}
