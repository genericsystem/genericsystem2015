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

	protected Handler<Buffer> getHandler(Handler<Object> consumer) {
		return buffer -> {
			GSBuffer buff = new GSBuffer(buffer);
			int methodId = buff.getInt();
			switch (methodId) {
			case PICK_NEW_TS: {
				consumer.handle(buff.getLong());
				break;
			}
			case GET_DEPENDENCIES: {
				int size = buff.getInt();
				long[] result = new long[size];
				for (int i = 0; i < size; i++) {
					result[i] = buff.getLong();
				}
				consumer.handle(result);
				break;
			}
			case GET_VERTEX: {
				consumer.handle(buff.getGSVertex());
				break;
			}
			case APPLY: {
				consumer.handle(buff.getLong());
				break;
			}
			default:
				throw new IllegalStateException("no method called");
			}
		};
	}

	public <T> T synchronize(Buffer buffer) {
		try {
			return unsafeSynchronize(buffer);
		} catch (OptimisticLockConstraintViolationException | ConcurrencyControlException e) {
			throw new IllegalStateException(e);
		}
	}

	public <T> T unsafeSynchronize(Buffer buffer) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		return synchonizeTask(objectHandler -> {
			send(buffer, reponse -> {
				reponse.bodyHandler(getHandler(object -> {
					objectHandler.handle(reponse.statusCode() == 200 ? object : reponse.statusCode() == 400 ? new ConcurrencyControlException("") : new OptimisticLockConstraintViolationException(""));
				}));
			});
		});
	}

	private static <T> T synchonizeTask(Handler<Handler<Object>> consumer) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(1);
			consumer.handle(res -> {
				try {
					blockingQueue.put(res);
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
					throw new ConcurrencyControlException("reaching synchronize");
				else if (result instanceof OptimisticLockConstraintViolationException)
					throw new OptimisticLockConstraintViolationException("");
				return (T) result;
			}
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	abstract void send(Buffer buffer, Handler<HttpClientResponse> handler);

	@Override
	public Vertex getVertex(long id) {
		Vertex result = (Vertex) synchronize(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id));
		if (result == null)
			throw new IllegalStateException("Vertex id: " + id);
		return result;
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return (long[]) synchronize(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id));
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
		unsafeSynchronize(gsBuffer);
	}

	@Override
	public long pickNewTs() {
		return (Long) synchronize(Buffer.buffer().appendInt(PICK_NEW_TS));
	}

	@Override
	public abstract void close();
}
