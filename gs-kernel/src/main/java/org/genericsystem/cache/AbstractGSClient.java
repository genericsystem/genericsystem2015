package org.genericsystem.cache;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.Arrays;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.ClientCacheProtocole;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSClient implements ClientCacheProtocole {

	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;

	abstract <T> void send(Buffer buffer, Handler<Buffer> reponseHandler);

	@Override
	public Vertex getVertex(long id) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), buff -> {
			task.handle(new GSBuffer(buff).getGSVertexArray());
		}));

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
		Long receivedTs = (Long) synchonizeTask(task -> send(gsBuffer, buff -> task.handle(new GSBuffer(buff).getLong())));
		if (receivedTs == Statics.CONCURRENCY_CONTROL_EXCEPTION)
			throw new ConcurrencyControlException("");
		else if (receivedTs == Statics.OTHER_EXCEPTION)
			throw new OptimisticLockConstraintViolationException("");
	}

	@Override
	public long pickNewTs() {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(PICK_NEW_TS), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	protected static <T> T synchonizeTask(Consumer<Handler<T>> consumer) {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<T> blockingQueue = new ArrayBlockingQueue<>(1);
			consumer.accept(resultObject -> {
				try {
					blockingQueue.put(resultObject);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			});
			T result = null;
			try {
				result = blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			if (result != null) {
				// try {
				// Thread.sleep(50);
				// } catch (InterruptedException e) {
				// // TODO Auto-generated catch block
				// e.printStackTrace();
				// }
				return result;
			}
			System.out.println("Response failure");
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	@Override
	public abstract void close();
}
