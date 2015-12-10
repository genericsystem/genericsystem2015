package org.genericsystem.distributed;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.RollbackException;
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

	protected WebSocketClient webSocketClient;

	public AbstractGSClient(String host, int port, String path) {
		webSocketClient = new WebSocketClient(this, host, port, path);
	}

	protected <T> void send(Buffer buffer) {
		webSocketClient.send(buffer);
	}

	@Override
	public void close() {
		webSocketClient.close();
	}

	private final Map<Integer, Consumer<GSBuffer>> ops = new HashMap<>();
	private final AtomicInteger atomicKey = new AtomicInteger(0);

	private final Handler<Buffer> handler = buf -> {
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
		return promise.thenApplyAsync(p -> p);
	}

	protected <T, R> R unsafeRollbackManaged(CompletableFuture<R> unsafe) {
		try {
			return unsafe.get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			if (ExecutionException.class.isAssignableFrom(e.getClass()) && e.getCause() != null && RollbackException.class.isAssignableFrom(e.getCause().getClass()))
				throw (RollbackException) e.getCause();
			throw new IllegalStateException(e);
		}
	}

	protected <T, R> R unsafeRollbackAndConcurrencyControlExceptionManaged(CompletableFuture<R> unsafe) throws ConcurrencyControlException {
		try {
			return unsafe.get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			if (ExecutionException.class.isAssignableFrom(e.getClass()) && e.getCause() != null)
				if (RollbackException.class.isAssignableFrom(e.getCause().getClass()))
					throw (RollbackException) e.getCause();
			if (ConcurrencyControlException.class.isAssignableFrom(e.getCause().getClass()))
				throw (ConcurrencyControlException) e.getCause();
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("unchecked")
	protected <R> CompletableFuture<R> unsafeExceptionPromise(CompletableFuture<Object> unsafe) {
		assert !Thread.currentThread().toString().subSequence(0, 13).equals("Thread[vert.x");

		CompletableFuture<R> cf = new CompletableFuture<>();
		((CompletableFuture<R>) unsafe).thenApply(res -> {
			if (Exception.class.isAssignableFrom(res.getClass()))
				return cf.completeExceptionally((Exception) res);
			return cf.complete(res);
		});
		return cf;
	}

	@Override
	public Vertex getVertex(long id) {
		return unsafeRollbackManaged(getVertexPromise(id));
	}

	public CompletableFuture<Vertex> getVertexPromise(long id) {
		return unsafeExceptionPromise(promise(GET_VERTEX, buff -> buff.getGSVertexThrowException(), buffer -> buffer.appendLong(id)));
	}

	@Override
	public long pickNewTs() {
		return unsafeRollbackManaged(getPickNewTsPromise());
	}

	public CompletableFuture<Long> getPickNewTsPromise() {
		return unsafeExceptionPromise(promise(PICK_NEW_TS, buff -> buff.getLongThrowException(), buffer -> buffer));
	}

	// protected static <T> T synchronizeTaskWithException(Consumer<Handler<Object>> consumer) throws ConcurrencyControlException {
	// for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
	// BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(Statics.HTTP_ATTEMPTS);
	// consumer.accept(resultObject -> {
	// try {
	// // System.out.println("Free BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread() + " " + resultObject);
	// blockingQueue.put(resultObject);
	// /*
	// * globalResultContainer.put(resultObject) ; return;
	// */
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// });
	// Object result = null;
	// try {
	// // System.out.println("Poll BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread());
	// result = blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// if (result != null) {
	// if (result instanceof RuntimeException)
	// throw (RuntimeException) result;
	// if (result instanceof ConcurrencyControlException)
	// throw (ConcurrencyControlException) result;
	// else
	// return (T) result;
	// }
	//
	// System.out.println("Response failure " + Thread.currentThread());
	// }
	// throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	// }

	// protected static <T> T synchronizeTask(Consumer<Handler<Object>> consumer) {
	// for (int i = 0; i < Statics.HTTP_ATTEMPTS; i++) {
	//
	// CompletableFuture<T> promise = CompletableFuture.supplyAsync(() -> {
	// T result = null;
	//
	// return result;
	// });
	//
	// BlockingQueue<Object> blockingQueue = new ArrayBlockingQueue<>(Statics.HTTP_ATTEMPTS);
	// consumer.accept(resultObject -> {
	// try {
	// // System.out.println("Free BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread() + " " + resultObject);
	// blockingQueue.put(resultObject);
	// /*
	// * globalResultContainer.put(resultObject) ; return;
	// */
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// });
	// Object result = null;
	// try {
	// // System.out.println("Poll BlockingQueue : " + System.identityHashCode(blockingQueue) + " " + Thread.currentThread());
	// result = blockingQueue.poll(2000, TimeUnit.MILLISECONDS);
	// } catch (InterruptedException e) {
	// e.printStackTrace();
	// }
	// if (result != null) {
	// if (result instanceof RuntimeException)
	// throw (RuntimeException) result;
	// else
	// return (T) result;
	// }
	//
	// System.out.println("Response failure " + Thread.currentThread());
	// }
	// throw new IllegalStateException("Unable get reponse for " + Statics.HTTP_ATTEMPTS + " times");
	// }

}
