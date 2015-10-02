package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.kernel.LightServerEngine;
import org.genericsystem.kernel.Statics;

public abstract class AbstractLightGSServer extends AbstractGSServer {

	public AbstractLightGSServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, LightServerEngine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer();
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
		case AbstractGSClient.APPLY:
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
