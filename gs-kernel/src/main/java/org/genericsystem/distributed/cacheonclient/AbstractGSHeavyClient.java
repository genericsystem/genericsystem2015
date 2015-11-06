package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.buffer.Buffer;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;

public abstract class AbstractGSHeavyClient extends AbstractGSClient implements ClientCacheProtocole {

	@Override
	public Vertex getVertex(long id) {
		CompletableFuture<Vertex> promise = new CompletableFuture<>();
		send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> {
			promise.complete(new GSBuffer(buff).getGSVertex());
		});
		try {
			return promise.get();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		// return synchronizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	// @Override
	// public Vertex[] getDependencies(long ts, long id) {
	// return synchronizeTask(task -> send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), buff -> {
	// task.handle(new GSBuffer(buff).getGSVertexArray());
	// }));
	// }

	@Override
	public Vertex[] getDependencies(long ts, long id) {

		CompletableFuture<Vertex[]> promise = new CompletableFuture<>();
		send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), buff -> {
			promise.complete(new GSBuffer(buff).getGSVertexArray());
		});
		try {
			return promise.get();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

	}

	@Override
	public void sendRequestForObservableDependencies(Consumer container, HeavyClientEngine root, long ts, long id) {

		send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), buff -> {
			container.accept(Arrays.stream(new GSBuffer(buff).getGSVertexArray()).map(vertex -> root.getGenericByVertex(vertex)).collect(Collectors.toList()));
		});

	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");
		GSBuffer gsBuffer = new GSBuffer();
		gsBuffer.appendInt(APPLY);
		gsBuffer.appendLong(ts);
		gsBuffer.appendGSLongArray(removes);
		gsBuffer.appendGSVertexArray(adds);
		CompletableFuture<Object> promise = new CompletableFuture<>();
		send(gsBuffer, buff -> promise.complete(new GSBuffer(buff).getLongThrowException()));

		Object res;
		try {
			res = promise.get();
		} catch (InterruptedException | ExecutionException e) {
			throw new RuntimeException(e);
		}
		// so as to be sent up
		if (res instanceof OptimisticLockConstraintViolationException)
			throw (OptimisticLockConstraintViolationException) res;
		if (res instanceof ConcurrencyControlException)
			throw (ConcurrencyControlException) res;
		if (res instanceof Throwable)
			throw new IllegalStateException((Throwable) res);
	}

	// to desync
	@Override
	public long pickNewTs() {
		// CompletableFuture<Long> promise = new CompletableFuture<>();
		// send(Buffer.buffer().appendInt(PICK_NEW_TS), buff -> {
		// promise.complete(new GSBuffer(buff).getLong());
		// });
		// try {
		// return promise.get();
		// } catch (Exception e) {
		// throw new RuntimeException(e);
		// }
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(PICK_NEW_TS), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

}
