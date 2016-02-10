package org.genericsystem.distributed;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.buffer.impl.BufferFactoryImpl;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.genericsystem.kernel.AbstractServer;

public class WebSocketServer<T extends AbstractServer> {
	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;
	private AbstractGSServer<T> server;

	public WebSocketServer(AbstractGSServer<T> server, GSDeploymentOptions options) {
		this.server = server;
		this.port = options.getPort();
		this.host = options.getHost();
	}

	// @SuppressWarnings("unchecked")
	public void start(Map<String, AbstractServer> roots) {
		Vertx vertx = GSVertx.vertx().getVertx();
		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
			httpServer.websocketHandler(webSocket -> {
				String path = webSocket.path();
				AbstractServer root = roots.get(path);
				if (root == null)
					throw new IllegalStateException("Unable to find database :" + path);
				webSocket.exceptionHandler(e -> {

					e.printStackTrace();
					throw new IllegalStateException(e);
				});

				webSocket.handler(buffer -> {
					Buffer buf = new BufferFactoryImpl().buffer(buffer.getByteBuf().order(ByteOrder.LITTLE_ENDIAN));
					GSBuffer gsBuffer = new GSBuffer(buf);
					int methodId = gsBuffer.getInt();
					int op = gsBuffer.getInt();
					webSocket.writeBinaryMessage(server.getReplyBuffer(methodId, op, (T) root, gsBuffer));
				});

				// Byte methodId = buffer.getByte(0);
				// String nodeId = buffer.getString(1, buffer.length());
				// System.out.println("ZZZZZZZZZZZ" + methodId + " " + nodeId);
				// Buffer buf = new BufferFactoryImpl().buffer(Buffer.buffer().getByteBuf().order(ByteOrder.LITTLE_ENDIAN));
				//
				// buf.appendInt(54);
				// webSocket.writeBinaryMessage(buf);
				// int op = gsBuffer.getInt();
				// webSocket.writeBinaryMessage(server.getReplyBuffer(methodId, op, (T) root, gsBuffer));
				// });

				});
			AbstractGSServer.<HttpServer> synchronizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
		System.out.println("Generic System server ready!");
	}

	public void stop(Map<String, AbstractServer> roots) {
		httpServers.forEach(httpServer -> AbstractGSServer.<Void> synchronizeTask(handler -> httpServer.close(handler)));
		roots.values().forEach(root -> root.close());
		roots = null;
		System.out.println("Generic System server stopped!");
	}

}
