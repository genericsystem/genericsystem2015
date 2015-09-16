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

	abstract <T> void send(Buffer buffer, Handler<Buffer> reponseHandler);

	@Override
	public Vertex getVertex(long id) {
		return synchonizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
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
