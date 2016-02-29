package org.genericsystem.distributed.cacheonclient;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractFrontEnd;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.kernel.Statics;

public class FrontEnd extends AbstractFrontEnd implements CocProtocol {

	public FrontEnd(String host, int port, String path) {
		super(host, port, path);
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		return extractRuntimeException(() -> getDependenciesPromise(ts, id).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));

	}

	public CompletableFuture<Vertex[]> getDependenciesPromise(long ts, long id) {
		return extractRuntimeExceptionPromise(() -> promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArrayThrowException(), buffer -> buffer.appendLong(ts).appendLong(id)));
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");
		Object res = extractRuntimeException(() -> applyPromise(ts, removes, adds).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
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
