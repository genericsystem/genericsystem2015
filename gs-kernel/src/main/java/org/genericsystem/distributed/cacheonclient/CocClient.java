package org.genericsystem.distributed.cacheonclient;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.kernel.Statics;

public class CocClient extends AbstractGSClient implements CocProtocole {

	public CocClient(String host, int port, String path) {
		super(host, port, path);
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		return unsafeRollbackManaged(getDependenciesPromise(ts, id));

	}

	public CompletableFuture<Vertex[]> getDependenciesPromise(long ts, long id) {
		return unsafeExceptionPromise(promise(GET_DEPENDENCIES, buff -> buff.getGSVertexArrayThrowException(), buffer -> buffer.appendLong(ts).appendLong(id)));
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		if (!Arrays.stream(adds).allMatch(v -> (v.getBirthTs() == Long.MAX_VALUE)))
			throw new IllegalStateException("");

		try {
			applyPromise(ts, removes, adds).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			if (ExecutionException.class.isAssignableFrom(e.getClass()) && e.getCause() != null)
				if (OptimisticLockConstraintViolationException.class.isAssignableFrom(e.getCause().getClass()))
					throw (OptimisticLockConstraintViolationException) e.getCause();
				else if (ConcurrencyControlException.class.isAssignableFrom(e.getCause().getClass()))
					throw (ConcurrencyControlException) e.getCause();
			throw new IllegalStateException(e);
		}
	}

	public CompletableFuture<Object> applyPromise(long ts, long[] removes, Vertex[] adds) {
		return unsafeExceptionPromise(promise(APPLY, buff -> buff.getLongThrowException(), buffer -> {
			GSBuffer gsBuffer = new GSBuffer(buffer);
			gsBuffer.appendLong(ts);
			gsBuffer.appendGSLongArray(removes);
			gsBuffer.appendGSVertexArray(adds);
			return gsBuffer;
		}));
	}

}
