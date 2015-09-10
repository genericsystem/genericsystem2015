package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientResponse;

import java.util.Arrays;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSClient implements Server {

	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;

	public <T> T synchronizeSend(Buffer buffer, int methodId) {
		try {
			return unsafeSynchronizeSend(buffer, methodId);
		} catch (OptimisticLockConstraintViolationException | ConcurrencyControlException e) {
			throw new IllegalStateException(e);
		}
	}

	public <T> T unsafeSynchronizeSend(Buffer buffer, int methodId) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		return synchonizeTask(task -> send(buffer, reponse -> {
			if (reponse.statusCode() == 200) {
				switch (methodId) {
				case PICK_NEW_TS: {
					reponse.bodyHandler(buff -> {
						task.handle(new GSBuffer(buff).getLong());
					});
					break;
				}
				case GET_DEPENDENCIES: {
					reponse.bodyHandler(buff -> {
						GSBuffer gsBuffer = new GSBuffer(buff);
						int size = gsBuffer.getInt();
						long[] result = new long[size];
						for (int i = 0; i < size; i++)
							result[i] = gsBuffer.getLong();
						task.handle(result);
					});
					break;
				}
				case GET_VERTEX: {
					reponse.bodyHandler(buff -> {
						task.handle(new GSBuffer(buff).getGSVertex());
					});
					break;
				}
				case APPLY: {
					reponse.bodyHandler(buff -> {
						task.handle(new GSBuffer(buff).getLong());
					});
					break;
				}
				default:
					throw new IllegalStateException("no method called");
				}
			} else
				task.handle(reponse.statusCode() == 400 ? new ConcurrencyControlException("") : new OptimisticLockConstraintViolationException(""));
		}));
	}

	@SuppressWarnings("unchecked")
	private static <T> T synchonizeTask(Handler<Handler<Object>> consumer) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(1);
			consumer.handle(resultObject -> {
				try {
					blockingQueue.put(resultObject);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			});
			Object result = null;
			try {
				result = blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			if (result != null) {
				if (result instanceof ConcurrencyControlException)
					throw (ConcurrencyControlException) result;
				else if (result instanceof OptimisticLockConstraintViolationException)
					throw (OptimisticLockConstraintViolationException) result;
				return (T) result;
			}
			System.out.println("Response failure");
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	abstract void send(Buffer buffer, Handler<HttpClientResponse> handler);

	@Override
	public Vertex getVertex(long id) {
		Vertex result = (Vertex) synchronizeSend(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), GET_VERTEX);
		if (result == null)
			throw new IllegalStateException("Vertex id: " + id);
		return result;
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return (long[]) synchronizeSend(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), GET_DEPENDENCIES);
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		GSBuffer gsBuffer = new GSBuffer(Buffer.buffer());
		gsBuffer.appendInt(APPLY);
		gsBuffer.appendLong(ts);
		gsBuffer.appendGSLongArray(removes);
		gsBuffer.appendGSVertexArray(adds);
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");
		unsafeSynchronizeSend(gsBuffer, APPLY);
	}

	@Override
	public long pickNewTs() {
		return (Long) synchronizeSend(Buffer.buffer().appendInt(PICK_NEW_TS), PICK_NEW_TS);
	}

	@Override
	public abstract void close();
}
