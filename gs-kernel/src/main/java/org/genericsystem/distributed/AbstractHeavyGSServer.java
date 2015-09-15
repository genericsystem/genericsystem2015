package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;

public abstract class AbstractHeavyGSServer extends AbstractGSServer {
	public AbstractHeavyGSServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, Root root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer(Buffer.buffer());
		switch (methodId) {
		case AbstractGSHeavyClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSHeavyClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSHeavyClient.GET_VERTEX:
			return replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
		case AbstractGSHeavyClient.APPLY:
			try {
				root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
				return replyBuffer.appendLong(0);
			} catch (Exception e) {
				e.printStackTrace();
				return replyBuffer.appendLong(e instanceof ConcurrencyControlException ? Statics.CONCURRENCY_CONTROL_EXCEPTION : Statics.OTHER_EXCEPTION);
			}
		default:
			throw new IllegalStateException("unable to find method:" + methodId + " ");
		}
	}
}
