package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.example.util.ExampleRunner;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Server;

public class VertxServerClient extends AbstractVerticle implements Server {
	private final Server root = new Root();
	public static final int SEND_BUFFER_SIZE = 32 * 1024;

	public static void main(String[] args) {
		ExampleRunner.runJavaExample("src/main/java/", VertxServerClient.class,
				false);
	}

	@Override
	public Vertex getVertex(long id) {
		return root.getVertex(id);
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return root.getDependencies(ts, id);
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds)
			throws ConcurrencyControlException,
			OptimisticLockConstraintViolationException {
		root.apply(ts, removes, adds);
	}

	@Override
	public long pickNewTs() {
		return root.pickNewTs();
	}

	@Override
	public void close() {
		root.close();
	}

	@Override
	public void start() {
		HttpServerOptions options = new HttpServerOptions()
				.setMaxWebsocketFrameSize(1000000).setSendBufferSize(
						SEND_BUFFER_SIZE);
		HttpServer httpServer = vertx.createHttpServer(options);
		httpServer.websocketHandler(webSocket -> {
			webSocket.handler(buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				int id = gsBuffer.getInt();
				int methodId = gsBuffer.getInt();
				GSBuffer buff = new GSBuffer(Buffer.buffer());
				buff.appendInt(id).appendInt(methodId);
				switch (methodId) {
				case VertxClientServer.PICK_NEW_TS: {
					buff.appendLong(pickNewTs());
					break;
				}
				case VertxClientServer.GET_DEPENDENCIES: {
					buff.appendGSLongArray(getDependencies(gsBuffer.getLong(),
							gsBuffer.getLong()));
					break;
				}
				case VertxClientServer.GET_VERTEX: {
					buff.appendGSVertex(getVertex(gsBuffer.getLong()));
					break;
				}
				case VertxClientServer.APPLY: {
					try {
						apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(),
								gsBuffer.getGSVertexArray());
					} catch (Exception e) {
						throw new IllegalStateException("");
					}
					break;
				}
				default:
					throw new IllegalStateException("unable to find method:"
							+ methodId + " " + "id :" + id);
				}
				webSocket.writeBinaryMessage(buff);
			});
		});
		httpServer.listen(8081);
		System.out.println("Receiver ready!");
	}
}
