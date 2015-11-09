package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.buffer.Buffer;

import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.AbstractGSServer;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.LightServerEngine;

public abstract class AbstractGSLightServer extends AbstractGSServer {

	public AbstractGSLightServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, int op, LightServerEngine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer();
		replyBuffer.appendInt(op);
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
		case AbstractGSClient.APPLY:
			// try {
			return replyBuffer.appendLongThrowException(() -> {
				root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
				return 0L;
			});
			// } catch (Exception e) {
			// e.printStackTrace();
			// return replyBuffer.appendLong(e instanceof ConcurrencyControlException ? Statics.CONCURRENCY_CONTROL_EXCEPTION : Statics.OTHER_EXCEPTION);
			// }
		default:
			throw new IllegalStateException("unable to find method:" + methodId + " ");
		}
	}
}
