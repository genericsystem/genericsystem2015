package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import org.genericsystem.distributed.AbstractBackEnd;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.WebSocketsServer;
import org.genericsystem.distributed.cacheonserver.todomvc.TodoApp;
import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.kernel.Engine;

public class BackEnd extends AbstractBackEnd<Engine> {

	public static void main(String[] args) {
		new BackEnd(new GSDeploymentOptions()).start();
	}

	public BackEnd(GSDeploymentOptions options) {
		super(options);
	}

	@Override
	protected Engine buildRoot(String value, String persistentDirectoryPath, Class[] userClasses) {
		return new Engine(value, persistentDirectoryPath, userClasses);
	}

	@Override
	protected WebSocketsServer<Engine> buildWebSocketsServer(GSDeploymentOptions options) {
		return new WebSocketsServer<Engine>(this, options.getHost(), options.getPort()) {
			@Override
			public Handler<Buffer> getHandler(Engine root, ServerWebSocket socket) {
				TodoApp todoListApp = new TodoApp(root, socket);
				return buffer -> {
					GSBuffer gsBuffer = new GSBuffer(buffer);
					String message = gsBuffer.getString(0, gsBuffer.length());
					JsonObject json = new JsonObject(message);
					HtmlDomNode node = todoListApp.getNodeById(json.getString(HtmlDomNode.ID));
					if (node != null)
						node.handleMessage(json);
				};
			};
		};
	}
}
