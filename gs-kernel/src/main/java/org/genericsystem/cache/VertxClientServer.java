package org.genericsystem.cache;

import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.WebSocket;

import java.io.Serializable;
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

public class VertxClientServer implements Server {

	private final ClientEngine engine;
	private final WebSocket webSocket;
	private final AtomicInteger requestId = new AtomicInteger(0);
	private final Map<Integer, CountDownLatch> countDownLatchs = new ConcurrentHashMap<>();
	private final Map<Integer, Object> results = new ConcurrentHashMap<>();
	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;
	public static final int RECEIVE_BUFFER_SIZE = 32 * 1024;

	VertxClientServer(ClientEngine engine) {

		CountDownLatch cdl = new CountDownLatch(1);
		WebSocket[] socketArray = new WebSocket[1];
		this.engine = engine;

		Vertx.vertx()
				.createHttpClient(
						new HttpClientOptions()
								.setDefaultPort(8081)
								.setMaxWebsocketFrameSize(1000000)
								.setReceiveBufferSize(RECEIVE_BUFFER_SIZE)
								.setSendBufferSize(
										VertxServerClient.SEND_BUFFER_SIZE))
				.websocket("/gs", socket -> {
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
		webSocket.handler(buffer -> {
			GSBuffer buff = new GSBuffer(buffer);
			int id = buff.getInt();
			int methodId = buff.getInt();
			switch (methodId) {
			case PICK_NEW_TS: {
				results.put(id, buff.getLong());
				break;
			}
			case GET_DEPENDENCIES: {
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

				break;
			}
			default:
				throw new IllegalStateException("no method called");
			}
			countDownLatchs.get(id).countDown();
		});
	}

	@Override
	public Vertex getVertex(long id) {
		Buffer buff = Buffer.buffer().appendLong(id);
		Vertex result = (Vertex) synchronize(GET_VERTEX, buff);
		if (result == null)
			throw new IllegalStateException("Vertex id: " + id);
		return result;
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		Buffer buff = Buffer.buffer().appendLong(ts).appendLong(id);
		return (long[]) synchronize(GET_DEPENDENCIES, buff);
	}

	@SuppressWarnings("unchecked")
	public <T> T synchronize(int methodId, Buffer parameters) {
		int id = requestId.incrementAndGet();
		final CountDownLatch countDownLatch = new CountDownLatch(1);
		CountDownLatch oldCountDownLatch = countDownLatchs.put(id,
				countDownLatch);
		assert oldCountDownLatch == null;
		Buffer buffer = Buffer.buffer().appendInt(id).appendInt(methodId)
				.appendBuffer(parameters);

		webSocket.writeBinaryMessage(buffer);
		try {
			countDownLatch.await(5000, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			engine.getCurrentCache().discardWithException(e);
		}
		T result = (T) results.get(id);
		countDownLatchs.remove(id);
		results.remove(id);
		return result;
	}

	public static class Apply implements Serializable {

		private static final long serialVersionUID = 8725793789149242073L;
		public long ts;
		public long[] removes;
		public Vertex[] adds;

		Apply(long ts, long[] removes, Vertex[] adds) {
			this.ts = ts;
			this.removes = removes;
			this.adds = adds;
		}
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds)
			throws ConcurrencyControlException,
			OptimisticLockConstraintViolationException {
		GSBuffer gsBuffer = new GSBuffer(Buffer.buffer());
		gsBuffer.appendLong(ts);
		gsBuffer.appendGSLongArray(removes);
		gsBuffer.appendGSVertexArray(adds);
		assert Arrays.stream(adds).allMatch(
				add -> add.getOtherTs()[0] == Long.MAX_VALUE);
		if (Arrays.stream(adds).anyMatch(
				v -> (v.getOtherTs()[0] != Long.MAX_VALUE)))
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
