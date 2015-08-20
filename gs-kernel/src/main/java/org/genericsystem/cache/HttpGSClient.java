package org.genericsystem.cache;

import io.vertx.core.AsyncResult;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import java.util.Arrays;
import java.util.Map;
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
	private final AtomicInteger requestId = new AtomicInteger(0);
	private final Map<Integer, CountDownLatch> countDownLatchs = new ConcurrentHashMap<>();
	private final Map<Integer, Object> results = new ConcurrentHashMap<>();
	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;
	public static final int RECEIVE_BUFFER_SIZE = 32 * 1024;

	HttpGSClient(ClientEngine engine, String host, int port, String path) {
		CountDownLatch cdl = new CountDownLatch(1);
		WebSocket[] socketArray = new WebSocket[1];
		this.engine = engine;
		this.vertx = Vertx.vertx();
		vertx.createHttpClient(new HttpClientOptions().setDefaultPort(port).setDefaultHost(host != null ? host : HttpClientOptions.DEFAULT_DEFAULT_HOST))

		.websocket(path, socket -> {
			socket.handler(buffer -> {
				GSBuffer buff = new GSBuffer(buffer);
				int id = buff.getInt();
				int methodId = buff.getInt();
				switch (methodId) {
				case PICK_NEW_TS: {
					results.put(id, buff.getLong());
					break;
				}
				case GET_DEPENDENCIES: {
					// System.out.println(">>>Client side getdependencies response" + id);
					int size = buff.getInt();
					long[] result = new long[size];
					for (int i = 0; i < size; i++) {
						result[i] = buff.getLong();
					}
					results.put(id, result);
					break;
				}
				case GET_VERTEX: {
					results.put(id, buff.getGSVertex());
					break;
				}
				case APPLY: {
					buff.getInt();
					break;
				}
				default:
					throw new IllegalStateException("no method called");
				}
				// System.out.println("countDown " + id + " " + results.get(id));
				countDownLatchs.get(id).countDown();

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
		int id = requestId.incrementAndGet();
		final CountDownLatch mainCountDownLatch = new CountDownLatch(1);
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		CountDownLatch oldCountDownLatch = countDownLatchs.put(id, countDownLatch);

		assert oldCountDownLatch == null;
		Buffer buffer = Buffer.buffer().appendInt(id).appendInt(methodId).appendBuffer(parameters);

		// System.out.println("before writeFinalBinaryFrame(buffer) : " + id);
		assert !webSocket.writeQueueFull();
		AsyncResult<T>[] asyncResult = new AsyncResult[1];
		vertx.executeBlocking(future -> {
			webSocket.writeBinaryMessage(buffer);
			// System.out.println("after writeFinalBinaryFrame(buffer) : " + id);
				boolean latchResult = false;
				try {
					// System.out.println(">>> waiting for : " + id + " " + System.identityHashCode(countDownLatch));
					latchResult = countDownLatch.await(5000, TimeUnit.MILLISECONDS);
				} catch (InterruptedException e) {
					engine.getCurrentCache().discardWithException(e);
				}
				if (!latchResult) {
					engine.getCurrentCache().discardWithException(new IllegalStateException("No response received from server during waiting time"));
				}
				T result = (T) results.get(id);
				countDownLatchs.remove(id);
				results.remove(id);
				future.complete(result);
			}, res -> {
				asyncResult[0] = (AsyncResult<T>) res;
				mainCountDownLatch.countDown();
			});
		try {
			mainCountDownLatch.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
			Thread.currentThread().interrupt();
		}
		return asyncResult[0].result();
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
