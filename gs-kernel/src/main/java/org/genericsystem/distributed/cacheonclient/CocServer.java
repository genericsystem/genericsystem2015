package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.buffer.Buffer;

import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.AbstractGSServer;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.BasicEngine;

public class CocServer extends AbstractGSServer {

	public static void main(String[] args) {
		new CocServer(new GSDeploymentOptions()).start();
	}
	
	public CocServer(GSDeploymentOptions options) {
		super(options);
	}

	@Override
	protected Buffer getReplyBuffer(int methodId, int op, AbstractServer basicEngine, GSBuffer gsBuffer) {
		
		BasicEngine root = (BasicEngine)basicEngine;
		
		GSBuffer replyBuffer = new GSBuffer().appendInt(op);
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLongThrowException(() -> root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArrayThrowException(() -> root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));/////
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertexThrowException(() -> root.getVertex(gsBuffer.getLong()));
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

	@Override
	protected BasicEngine buildRoot(String value,
			String persistentDirectoryPath, Class[] userClasses) {
		return new BasicEngine(value, persistentDirectoryPath, userClasses);
	}
}
