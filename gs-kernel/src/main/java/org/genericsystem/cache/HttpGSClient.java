package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.kernel.Root;

public class HttpGSClient extends AbstractVerticle {

	// public static void main(String[] args) {
	// ExampleRunner.runJavaExample("src/main/java/", HttpGSClient.class,
	// false);
	// }

	private final Map<String, Root> roots = new ConcurrentHashMap<>();

	@Override
	public void start() {
		HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions()
				.setPort(config().getInteger("port")).setSendBufferSize(
						VertxClientServer.RECEIVE_BUFFER_SIZE));

		httpServer
				.websocketHandler(webSocket -> {
					String path = webSocket.path();
					Root root_ = roots.get(path);
					if (root_ == null) {
						Root result = roots.putIfAbsent(
								path,
								root_ = new Root(path.substring(1,
										path.length()), config().getString(
										"persistanceRepositoryPath")));
						if (result != null) {
							root_.close();
							root_ = result;
						}

					}

					final Root root = root_;
					webSocket.exceptionHandler(e -> {
						e.printStackTrace();
					});
					webSocket.handler(buffer -> {
						GSBuffer gsBuffer = new GSBuffer(buffer);
						int id = gsBuffer.getInt();
						int methodId = gsBuffer.getInt();
						GSBuffer buff = new GSBuffer(Buffer.buffer());
						buff.appendInt(id).appendInt(methodId);
						System.out.println(">>>: before server switch" + id);
						switch (methodId) {
						case VertxClientServer.PICK_NEW_TS: {

							buff.appendLong(root.pickNewTs());
							break;
						}
						case VertxClientServer.GET_DEPENDENCIES: {

							buff.appendGSLongArray(root.getDependencies(
									gsBuffer.getLong(), gsBuffer.getLong()));

							break;
						}
						case VertxClientServer.GET_VERTEX: {
							buff.appendGSVertex(root.getVertex(gsBuffer
									.getLong()));
							break;
						}
						case VertxClientServer.APPLY: {
							try {
								root.apply(gsBuffer.getLong(),
										gsBuffer.getGSLongArray(),
										gsBuffer.getGSVertexArray());
							} catch (Exception e) {
								throw new IllegalStateException("");
							}
							break;
						}
						default:
							throw new IllegalStateException(
									"unable to find method:" + methodId + " "
											+ "id :" + id);
						}
						System.out.println(">>>>>>: after server switch" + id);
						webSocket.writeFinalBinaryFrame(buff);
					});

				});

		httpServer.listen();
		System.out.println("Receiver ready!");
	}
}
