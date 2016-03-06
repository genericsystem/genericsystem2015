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

import org.genericsystem.common.Protocole;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

import com.google.common.base.Supplier;

public abstract class AbstractFrontEnd implements Protocole {

	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;
	public static final int SHIFT_TS = 4;
	// public static final int ADD_INSTANCE = 5;
	// public static final int SET_INSTANCE = 6;
	// public static final int MERGE = 7;
	// public static final int UPDATE = 8;
	// public static final int REMOVE = 9;
	// public static final int FORCE_REMOVE = 10;
	// public static final int CONSERVE_REMOVE = 11;
	// public static final int TRY_FLUSH = 12;
	// public static final int FLUSH = 13;
	// public static final int MOUNT = 14;
	// public static final int UNMOUNT = 15;
	// public static final int GET_CACHE_LEVEL = 16;
	// public static final int NEW_CACHE = 17;
	// public static final int CLEAR = 18;
	// public static final int SUBELEMENTS = 19;

	protected WebSocketClient webSocketClient;

	public AbstractFrontEnd(String host, int port, String path) {
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
		// System.out.println("SENDING : " + method + " " + key);
		send(sendParams.apply(Buffer.buffer().appendInt(method).appendInt(key)));
		return promise.thenApplyAsync(p -> p);
	}

	@FunctionalInterface
	public interface UnsafeSupplier<R> {
		R supply() throws InterruptedException, ExecutionException, TimeoutException;
	}

	protected <T, R> R extractRuntimeException(UnsafeSupplier<R> unsafe) {
		try {
			return unsafe.supply();
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("unchecked")
	protected <R> R extractRuntimeExceptionPromise(Supplier<Object> unsafe) {
		Object result = unsafe.get();
		if (result instanceof RuntimeException)
			throw (RuntimeException) result;
		return (R) result;
	}

	@Override
	public Vertex getVertex(long id) {
		return extractRuntimeException(() -> getVertexPromise(id).get(/* Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT */));
	}

	public CompletableFuture<Vertex> getVertexPromise(long id) {
		return extractRuntimeExceptionPromise(() -> promise(GET_VERTEX, buff -> buff.getGSVertexThrowException(), buffer -> buffer.appendLong(id)));
	}

	@Override
	public long pickNewTs() {
		return extractRuntimeException(() -> getPickNewTsPromise().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public CompletableFuture<Long> getPickNewTsPromise() {
		return extractRuntimeExceptionPromise(() -> promise(PICK_NEW_TS, buff -> buff.getLongThrowException(), buffer -> buffer));
	}
}
