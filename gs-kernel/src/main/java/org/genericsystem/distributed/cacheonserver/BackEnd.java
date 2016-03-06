package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.event.ActionEvent;

import org.genericsystem.distributed.AbstractBackEnd;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.WebSocketsServer;
import org.genericsystem.distributed.cacheonserver.todomvc.TodoApp;
import org.genericsystem.distributed.cacheonserver.todomvc.TodoList;
import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlDomNode.CheckBoxHtmlDomNode;
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
				TodoApp todoListApp = new TodoApp(new TodoList(), socket);
				return buffer -> {
					GSBuffer gsBuffer = new GSBuffer(buffer);
					String message = gsBuffer.getString(0, gsBuffer.length());
					JsonObject obj = new JsonObject(message);

					HtmlDomNode node = todoListApp.getNodeById(obj.getString("nodeId"));
					if (node != null) {
						if (obj.getString("msg_type").equals("A"))
							node.getActionProperty().get().handle(new ActionEvent());

						if (obj.getString("msg_type").equals("U")) {
							if (obj.getString("eltType").equals("text"))
								node.getText().setValue(obj.getString("textContent"));

							if ("checkbox".equals(obj.getString("eltType")))
								((CheckBoxHtmlDomNode) node).getChecked().setValue(obj.getBoolean("checked"));
						}
					}
				};
			};
		};
	}
}
