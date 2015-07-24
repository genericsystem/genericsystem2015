package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.example.util.ExampleRunner;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Server;

public class VertxServerClient extends AbstractVerticle implements Server {
	private final Server root = new Root();
	private int index = 0;

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
		HttpServer httpServer = vertx.createHttpServer();
		httpServer.websocketHandler(webSocket -> {
			System.out.println("receive websocket from client");
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
				}
				default:
					throw new IllegalStateException("unable to find method:"
							+ methodId + " " + "id :" + id);
				}

				webSocket.writeBinaryMessage(buff);
			});
		});
		httpServer.listen(8081);

		// EventBus eb = vertx.eventBus();
		// eb.consumer("pickNewTs", message -> {
		// try {
		// // System.out.println("Receive picknewts");
		// message.reply(pickNewTs());
		// } catch (Throwable e) {
		// e.printStackTrace();
		// message.fail(400, e.getMessage());
		// }
		// });
		// eb.consumer(
		// "getDependencies",
		// message -> {
		// try {
		// // System.out.println("Receive getDependencies");
		// JsonObject json = (JsonObject) message.body();
		// message.reply(getDependencies(json.getLong("ts"),
		// json.getLong("id")));
		// // message.reply(new
		// // JsonArray(Arrays.asList(getDependencies(json.getLong("ts"),
		// // json.getLong("id")))));
		// } catch (Throwable e) {
		// e.printStackTrace();
		// message.fail(400, e.getMessage());
		// }
		// });
		// eb.consumer("getVertex", message -> {
		// try {
		// // System.out.println("Receive getVertex");
		// long id = (Long) message.body();
		// message.reply(getVertex(id));
		// } catch (Throwable e) {
		// e.printStackTrace();
		// message.fail(400, e.getMessage());
		// }
		// });
		// eb.consumer("apply", message -> {
		// try {
		// // System.out.println("Receive apply");
		// Apply apply = (Apply) message.body();
		// apply(apply.ts, apply.removes, apply.adds);
		// message.reply(null);
		// } catch (Throwable e) {
		// e.printStackTrace();
		// message.fail(400, e.getMessage());
		// }
		// });

		// @Override
		// public void encodeToWire(Buffer buffer, T serialilizable) {
		// ByteArrayOutputStream arrayStream = new ByteArrayOutputStream();
		// ObjectOutputStream s;
		// try {
		// s = new ObjectOutputStream(arrayStream);
		// s.writeObject(serialilizable);
		// s.flush();
		// } catch (IOException e) {
		// throw new IllegalStateException(e);
		// }
		// buffer.appendBytes(arrayStream.toByteArray());
		//
		// try {
		// T result = (T) new ObjectInputStream(new ByteArrayInputStream(
		// arrayStream.toByteArray())).readObject();
		// System.out.println("coucou" + result.getClass());
		//
		// } catch (ClassNotFoundException | IOException e) {
		// throw new IllegalStateException(name() + " : " + e);
		// }
		// }

		//
		// @SuppressWarnings("unchecked")
		// @Override
		// public T decodeFromWire(int pos, Buffer buffer) {
		// // throw new IllegalStateException(name());
		// try {
		// System.out.println("coucou pos : " + pos);
		// return (T) new ObjectInputStream(new ByteArrayInputStream(
		// buffer.getBytes(pos, buffer.length()))).readObject();
		// } catch (ClassNotFoundException | IOException e) {
		// throw new IllegalStateException(name() + " : " + e);
		// }
		// }

		System.out.println("Receiver ready!");
	}
}
