package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class HttpGSClient implements Server {

	private final ClientEngine engine;
	private final WebSocket webSocket;
	private final Vertx vertx;
	private static final AtomicInteger requestId = new AtomicInteger(0);
	private static final Map<Integer, BlockingQueue<Object>> bockingQueues = new ConcurrentHashMap<>();
	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;

	// public static final int RECEIVE_BUFFER_SIZE = 32 * 1024;

	HttpGSClient(ClientEngine engine, String host, int port, String path) {
		CountDownLatch cdl = new CountDownLatch(1);
		WebSocket[] socketArray = new WebSocket[1];
		this.engine = engine;
		this.vertx = Vertx.vertx();

		vertx.createHttpClient(new HttpClientOptions().setPipelining(false).setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST)).websocket(path, socket -> {
			socket.handler(buffer -> {
				GSBuffer buff = new GSBuffer(buffer);
				int id = buff.getInt();
				int methodId = buff.getInt();
				switch (methodId) {
				case PICK_NEW_TS: {
					try {
						bockingQueues.get(id).put(buff.getLong());
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					break;
				}
				case GET_DEPENDENCIES: {
					// System.out.println(">>>Client side getdependencies response" + id);
					int size = buff.getInt();
					long[] result = new long[size];
					for (int i = 0; i < size; i++) {
						result[i] = buff.getLong();
					}
					try {
						bockingQueues.get(id).put(result);
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					break;
				}
				case GET_VERTEX: {
					try {
						bockingQueues.get(id).put(buff.getGSVertex());
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					break;
				}
				case APPLY: {
					try {
						bockingQueues.get(id).put(buff.getLong());
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					break;
				}
				default:
					throw new IllegalStateException("no method called");
				}
			});
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

	@SuppressWarnings("unchecked")
	public <T> T synchronize(int methodId, Buffer parameters) {
		// try {
		// Thread.sleep(0, 1);
		// } catch (InterruptedException e1) {
		// e1.printStackTrace();
		// }

		T result = null;
		for (;;) {
			int id = requestId.getAndIncrement();
			final BlockingQueue<Object> b = new ArrayBlockingQueue<>(1);
			BlockingQueue<Object> oldBlockingQueue = bockingQueues.put(id, b);
			assert oldBlockingQueue == null;
			Buffer buffer = Buffer.buffer().appendInt(id).appendInt(methodId).appendBuffer(parameters);
			webSocket.writeFinalBinaryFrame(buffer);
			try {
				result = (T) b.poll(50, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				engine.getCurrentCache().discardWithException(e);
			}
			if (result != null) {
				bockingQueues.remove(id);
				return result;
			}
			System.out.println("Failure");
		}
	}

	@Override
	public Vertex getVertex(long id) {
		Vertex result = (Vertex) synchronize(GET_VERTEX, Buffer.buffer().appendLong(id));
		if (result == null)
			throw new IllegalStateException("Vertex id: " + id);
		return result;
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return (long[]) synchronize(GET_DEPENDENCIES, Buffer.buffer().appendLong(ts).appendLong(id));
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		GSBuffer gsBuffer = new GSBuffer(Buffer.buffer());
		gsBuffer.appendLong(ts);
		gsBuffer.appendGSLongArray(removes);
		gsBuffer.appendGSVertexArray(adds);
		assert Arrays.stream(adds).allMatch(add -> add.getOtherTs()[0] == Long.MAX_VALUE);
		if (Arrays.stream(adds).anyMatch(v -> (v.getOtherTs()[0] != Long.MAX_VALUE)))
			throw new IllegalStateException("");
		synchronize(APPLY, gsBuffer);
	}

	@Override
	public long pickNewTs() {
		return (Long) synchronize(PICK_NEW_TS, Buffer.buffer());
	}

	@Override
	public void close() {

	}
}
