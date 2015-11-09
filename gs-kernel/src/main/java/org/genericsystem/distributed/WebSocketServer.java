package org.genericsystem.distributed;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;

public class WebSocketServer {

	static Vertx vertx;
	static HttpServer httpServer;

	public static void main(String[] args) {
		vertx = Vertx.vertx();
		httpServer = vertx.createHttpServer();
		httpServer.websocketHandler(webSocket -> {
			webSocket.handler(buffer -> {
				int read = buffer.getByte(0);
				switch (read) {
				case '0':
					System.out.println("case 0");
					webSocket.writeBinaryMessage(Buffer.buffer().appendInt(read).appendInt(1).appendInt(2).appendInt(3));
					break;
				case '1':
					System.out.println("case 1");
					webSocket.writeBinaryMessage(Buffer.buffer().appendInt(read).appendInt(4).appendInt(5));
					break;
				case '5':
					System.out.println("case 5");
					webSocket.writeBinaryMessage(Buffer.buffer().appendInt(read).appendInt(6));
					break;
				default:
					System.out.println("case default");
					webSocket.writeBinaryMessage(Buffer.buffer().appendInt(read));
					break;
				}
			});
		});
		httpServer.listen(8081);
	}
}
