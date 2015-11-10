package org.genericsystem.distributed.cacheonclient;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.kernel.Statics;

public abstract class AbstractCocClient extends AbstractGSClient implements CocProtocole {

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		return (Vertex[]) unsafe(() -> getDependenciesPromise(ts, id).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));

	}

	public CompletableFuture<Object> getDependenciesPromise(long ts, long id) {
		return promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArrayThrowException(), buffer -> buffer.appendLong(ts).appendLong(id));
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");
		Object res = unsafe(() -> applyPromise(ts, removes, adds).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
		if (res instanceof OptimisticLockConstraintViolationException)
			throw (OptimisticLockConstraintViolationException) res;
		if (res instanceof ConcurrencyControlException)
			throw (ConcurrencyControlException) res;
		if (res instanceof Throwable)
			throw new IllegalStateException((Throwable) res);
	}

	public CompletableFuture<Object> applyPromise(long ts, long[] removes, Vertex[] adds) {
		return promise(APPLY, buff -> buff.getLongThrowException(), buffer -> {
			GSBuffer gsBuffer = new GSBuffer(buffer);
			gsBuffer.appendLong(ts);
			gsBuffer.appendGSLongArray(removes);
			gsBuffer.appendGSVertexArray(adds);
			return gsBuffer;
		});
	}

}
