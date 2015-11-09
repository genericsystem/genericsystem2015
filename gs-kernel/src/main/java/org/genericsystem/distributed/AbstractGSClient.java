package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
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

	protected abstract <T> void send(Buffer buffer);

	private final Map<Integer, Consumer<GSBuffer>> ops = new HashMap<>();
	private final AtomicInteger atomicKey = new AtomicInteger(0);

	private Handler<Buffer> handler = buf -> {
		GSBuffer gsBuffer = new GSBuffer(buf);
		int optype = gsBuffer.getInt();
		ops.remove(optype).accept(gsBuffer);
	};

	protected Handler<Buffer> getHandler() {
		return handler;
	}

	private int indexCallback(Consumer<GSBuffer> promise) {
		int key = atomicKey.incrementAndGet();
		ops.put(key, promise);
		return key;
	}

	protected <T> CompletableFuture<T> promise(int method, Function<GSBuffer, T> receiveReturn, Function<Buffer, Buffer> sendParams) {
		CompletableFuture<T> promise = new CompletableFuture<>();
		int key = indexCallback(buff -> promise.complete(receiveReturn.apply(buff)));
		send(sendParams.apply(Buffer.buffer().appendInt(method).appendInt(key)));
		return promise;
	}

	@FunctionalInterface
	public interface UnsafeSupplier<R> {
		R supply() throws InterruptedException, ExecutionException;
	}

	protected <T, R> R unsafe(UnsafeSupplier<R> unsafe) {
		try {
			return unsafe.supply();
		} catch (InterruptedException | ExecutionException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public Vertex getVertex(long id) {
		return unsafe(() -> getVertexPromise(id).get());
	}

	public CompletableFuture<Vertex> getVertexPromise(long id) {
		return promise(GET_VERTEX, buff -> buff.getGSVertex(), buffer -> buffer.appendLong(id));
	}

	@Override
	public long pickNewTs() {
		return unsafe(() -> getPickNewTsPromise().get());
	}

	public CompletableFuture<Long> getPickNewTsPromise() {
		return promise(PICK_NEW_TS, buff -> buff.getLong(), buffer -> buffer);
	}

	protected static <T> T synchronizeTaskWithException(Consumer<Handler<Object>> consumer) throws ConcurrencyControlException {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(Statics.HTTP_ATTEMPTS);
			consumer.accept(resultObject -> {
				try {
					// System.out.println("Free BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread() + " " + resultObject);
					blockingQueue.put(resultObject);
					/*
					 * globalResultContainer.put(resultObject) ; return;
					 */
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
				if (result instanceof ConcurrencyControlException)
					throw (ConcurrencyControlException) result;
				else
					return (T) result;
			}

			System.out.println("Response failure " + Thread.currentThread());
		}
		throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	}

	protected static <T> T synchronizeTask(Consumer<Handler<Object>> consumer) {
		for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {

			CompletableFuture<T> promise = CompletableFuture.supplyAsync(() -> {
				T result = null;

				return result;
			});

			BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(Statics.HTTP_ATTEMPTS);
			consumer.accept(resultObject -> {
				try {
					// System.out.println("Free BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread() + " " + resultObject);
					blockingQueue.put(resultObject);
					/*
					 * globalResultContainer.put(resultObject) ; return;
					 */
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
