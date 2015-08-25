package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.Arrays;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public abstract class AbstractGSClient implements Server {
	private final ClientEngine engine;
	private static final AtomicInteger requestId = new AtomicInteger(0);
	// private static final Map<Integer, BlockingQueue<Object>> bockingQueues =
	// new ConcurrentHashMap<>();
	BlockingQueue<Object> blockingQueue;
	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;

	protected Handler<Buffer> getHandler() {
		return buffer -> {
			GSBuffer buff = new GSBuffer(buffer);
			int id = buff.getInt();
			int methodId = buff.getInt();
			switch (methodId) {
			case PICK_NEW_TS: {
				try {
					blockingQueue.put(buff.getLong());
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				break;
			}
			case GET_DEPENDENCIES: {
				// System.out.println(">>>Client side getdependencies response"
				// + id);
				int size = buff.getInt();
				long[] result = new long[size];
				for (int i = 0; i < size; i++) {
					result[i] = buff.getLong();
				}
				try {
					blockingQueue.put(result);
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				break;
			}
			case GET_VERTEX: {
				try {
					blockingQueue.put(buff.getGSVertex());
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				break;
			}
			case APPLY: {
				try {
					blockingQueue.put(buff.getLong());
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				break;
			}
			default:
				throw new IllegalStateException("no method called");
			}
		};
	}

	AbstractGSClient(ClientEngine engine) {
		this.engine = engine;

	}

	@SuppressWarnings("unchecked")
	public <T> T synchronize(int methodId, Buffer parameters) {
		T result = null;
		for (;;) {
			int id = requestId.getAndIncrement();
			blockingQueue = new ArrayBlockingQueue<>(1);
			Buffer buffer = Buffer.buffer().appendInt(id).appendInt(methodId)
					.appendBuffer(parameters);
			send(buffer);
			try {
				result = (T) blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				engine.getCurrentCache().discardWithException(e);
			}

			if (result instanceof ConcurrencyControlException) {
				try {
					throw new ConcurrencyControlException("");
				} catch (ConcurrencyControlException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				blockingQueue = null;

			}
			if (result instanceof OptimisticLockConstraintViolationException) {

				try {
					throw new OptimisticLockConstraintViolationException("");
				} catch (OptimisticLockConstraintViolationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				blockingQueue = null;
			}
			if (result != null) {
				blockingQueue = null;
				return result;
			}
			System.out.println("Failure");
			// throw new IllegalStateException();// For now
		}
	}

	abstract void send(Buffer buffer);

	@Override
	public Vertex getVertex(long id) {
		Vertex result = (Vertex) synchronize(GET_VERTEX, Buffer.buffer()
				.appendLong(id));
		if (result == null)
			throw new IllegalStateException("Vertex id: " + id);
		return result;
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return (long[]) synchronize(GET_DEPENDENCIES, Buffer.buffer()
				.appendLong(ts).appendLong(id));
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
	public abstract void close();
}
