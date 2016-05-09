package org.genericsystem.distributed.cacheonclient;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;

import java.util.Collections;
import java.util.List;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.distributed.AbstractBackEnd;
import org.genericsystem.distributed.AbstractWebSocketsServer;
import org.genericsystem.distributed.EnginesDeploymentConfig;
import org.genericsystem.distributed.EnginesDeploymentConfig.DefaultPathSingleEngineDeployment;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.Engine;
import org.genericsystem.remote.FrontEnd;

/**
 * @author Nicolas Feybesse
 *
 */
public class EngineServer extends AbstractBackEnd {

	public static void main(String[] args) {
		new EngineServer(new DefaultPathSingleEngineDeployment("/", null)).start();
	}

	public EngineServer(EnginesDeploymentConfig options) {
		super(options.getHost(), options.getPort());
		System.out.println("Load config : \n" + options.encodePrettily());
		if (options.getEnginePaths().isEmpty()) {
			AbstractServer defaultRoot = buildRoot(null, Collections.emptyList());
			roots.put("/", defaultRoot);
			System.out.println("Starts engine with path : / and persistence repository path : null");
		} else
			for (String path : options.getEnginePaths()) {
				AbstractServer root = buildRoot(options.getPersistentDirectoryPath(path), options.getClasses(path));
				roots.put(path, root);
				System.out.println("Starts engine with path : " + path + " and persistence repository path : " + options.getPersistentDirectoryPath(path));
			}
	}

	protected AbstractServer buildRoot(String persistentDirectoryPath, List<Class<?>> userClasses) {
		return new Engine(persistentDirectoryPath, userClasses.stream().toArray(Class[]::new));
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

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {
			AbstractServer root = roots.get(path);
			if (root == null)
				throw new IllegalStateException("Unable to find database :" + path);

			AbstractCache cache = root.newCache();
			return buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				int methodId = gsBuffer.getInt();
				int op = gsBuffer.getInt();
				cache.safeConsum((x) -> socket.writeBinaryMessage(getReplyBuffer(methodId, op, root, gsBuffer)));
			};
		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
