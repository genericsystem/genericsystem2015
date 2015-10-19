package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.buffer.Buffer;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.GSBuffer;

public abstract class AbstractGSHeavyClient extends AbstractGSClient implements ClientCacheProtocole {

	@Override
	public Vertex getVertex(long id) {
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(GET_VERTEX).appendLong(id), buff -> task.handle(new GSBuffer(buff).getGSVertex())));
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(GET_DEPENDENCIES).appendLong(ts).appendLong(id), buff -> {
			task.handle(new GSBuffer(buff).getGSVertexArray());
		}));

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
		Object res = synchronizeTask(task -> send(gsBuffer, buff -> task.handle(new GSBuffer(buff).getLongThrowException())));
		// so as to be sent up
		if (res instanceof OptimisticLockConstraintViolationException) {
			throw (OptimisticLockConstraintViolationException) res;
		}
		if (res instanceof ConcurrencyControlException) {
			throw (ConcurrencyControlException) res;
		}
	}

	@Override
	public long pickNewTs() {
		return synchronizeTask(task -> send(Buffer.buffer().appendInt(PICK_NEW_TS), buff -> task.handle(new GSBuffer(buff).getLong())));
	}

}
