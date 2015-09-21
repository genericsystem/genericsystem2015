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

	abstract <T> void send(Buffer buffer, Handler<Buffer> reponseHandler);

	@Override
	public Vertex getVertex(long id) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
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
