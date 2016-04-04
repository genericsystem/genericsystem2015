package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.common.Cache;
import org.genericsystem.distributed.AbstractBackEnd;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.WebSocketsServer;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
public class BackEnd extends AbstractBackEnd<AbstractServer> {

	public static void main(String[] args) {
		new BackEnd(new GSDeploymentOptions()).start();
	}

	public BackEnd(GSDeploymentOptions options) {
		super(options);
	}

	protected Buffer getReplyBuffer(int methodId, int op, AbstractServer root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer().appendInt(op);
		// System.out.println("REPLY BUFFER : " + methodId + " " + op);
		switch (methodId) {
		case FrontEnd.PICK_NEW_TS:
			return replyBuffer.appendLongThrowException(() -> root.pickNewTs());
		case FrontEnd.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArrayThrowException(() -> root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case FrontEnd.GET_VERTEX:
			return replyBuffer.appendGSVertexThrowException(() -> root.getVertex(gsBuffer.getLong()));
		case FrontEnd.APPLY:
			return replyBuffer.appendLongThrowException(() -> {
				root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
				return 0L;
			});
		default:
			return replyBuffer.appendLongThrowException(() -> {
				throw new IllegalStateException("unable to find method:" + methodId + " ");
			});
		}
	}

	@Override
	protected AbstractServer buildRoot(String value, String persistentDirectoryPath, Class[] userClasses) {
		return new Engine(value, persistentDirectoryPath, userClasses);
	}

	@Override
	protected WebSocketsServer<AbstractServer> buildWebSocketsServer(GSDeploymentOptions options) {
		return new WebSocketsServer<AbstractServer>(this, options.getHost(), options.getPort()) {
			@Override
			public Handler<Buffer> getHandler(AbstractServer root, ServerWebSocket socket) {
				Cache cache = root.newCache();
				return buffer -> {
					GSBuffer gsBuffer = new GSBuffer(buffer);
					int methodId = gsBuffer.getInt();
					int op = gsBuffer.getInt();
					cache.start();
					socket.writeBinaryMessage(getReplyBuffer(methodId, op, root, gsBuffer));
					cache.stop();
				};
			}
		};
	}
}
