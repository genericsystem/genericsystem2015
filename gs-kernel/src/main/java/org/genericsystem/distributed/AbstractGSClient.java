package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import org.genericsystem.common.Protocole;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSClient implements Protocole {

	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;
	public static final int SHIFT_TS = 4;
	public static final int ADD_INSTANCE = 5;
	public static final int SET_INSTANCE = 6;
	public static final int MERGE = 7;
	public static final int UPDATE = 8;
	public static final int REMOVE = 9;
	public static final int FORCE_REMOVE = 10;
	public static final int CONSERVE_REMOVE = 11;
	public static final int TRY_FLUSH = 12;
	public static final int FLUSH = 13;
	public static final int MOUNT = 14;
	public static final int UNMOUNT = 15;
	public static final int GET_CACHE_LEVEL = 16;
	public static final int NEW_CACHE = 17;
	public static final int CLEAR = 18;

	protected abstract <T> void send(Buffer buffer, Handler<Buffer> reponseHandler);

	@Override
	public Vertex getVertex(long id) {
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	@Override
	public long pickNewTs() {
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(PICK_NEW_TS), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

	protected static <T> T synchronizeTask(Consumer<Handler<Object>> consumer) {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(Statics.HTTP_ATTEMPTS);
			consumer.accept(resultObject -> {
				try {
					// System.out.println("Free BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread() + " " + resultObject);
					blockingQueue.put(resultObject);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			});
			Object result = null;
			try {
				// System.out.println("Poll BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread());
				result = blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			if (result != null) {
				if (result instanceof RuntimeException)
					throw (RuntimeException) result;
				else
					return (T) result;
			}

			System.out.println("Response failure " + Thread.currentThread());
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	@Override
	public abstract void close();
}
