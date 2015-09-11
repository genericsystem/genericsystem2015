package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientResponse;

import java.util.Arrays;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

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

	abstract void send(Buffer buffer, Handler<HttpClientResponse> handler);

	@Override
	public Vertex getVertex(long id) {
		return (Vertex) synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), reponse -> reponse.bodyHandler(buff -> task.handle(new GSBuffer(buff).getGSVertex()))));
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return (long[]) synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), reponse -> reponse.bodyHandler(buff -> {
			GSBuffer gsBuffer = new GSBuffer(buff);
			int size = gsBuffer.getInt();
			long[] result = new long[size];
			for (int i = 0; i < size; i++)
				result[i] = gsBuffer.getLong();
			task.handle(result);
		})));
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");
		GSBuffer gsBuffer = new GSBuffer(Buffer.buffer());
		gsBuffer.appendInt(APPLY);
		gsBuffer.appendLong(ts);
		gsBuffer.appendGSLongArray(removes);
		gsBuffer.appendGSVertexArray(adds);
		
		Long receivedTs = synchronizeSend(gsBuffer, APPLY);
		if (receivedTs == Statics.CONCURRENCY_CONTROL_EXCEPTION)
			throw new ConcurrencyControlException("");
		else if (receivedTs == Statics.OTHER_EXCEPTION)
			throw new OptimisticLockConstraintViolationException("");
	}

	@Override
	public long pickNewTs() {
		return (Long) synchonizeTask(task -> send(Buffer.buffer().appendInt(PICK_NEW_TS), reponse -> reponse.bodyHandler(buff -> task.handle(new GSBuffer(buff).getLong()))));
	}

	private static Object synchonizeTask(Consumer<Handler<Object>> consumer) {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(1);
			consumer.accept(resultObject -> {
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
			if (result != null)
				return result;
			System.out.println("Response failure");
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	@Override
	public abstract void close();
}
