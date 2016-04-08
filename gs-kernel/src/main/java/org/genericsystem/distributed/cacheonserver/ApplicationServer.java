package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import java.util.Map.Entry;
import org.genericsystem.common.Cache;
import org.genericsystem.distributed.AbstractBackEnd;
import org.genericsystem.distributed.AbstractWebSocketsServer;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonserver.todomvc.TodoApp;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.HtmlDomNode;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.kernel.Engine;
import org.genericsystem.kernel.Statics;

/**
 * @author Nicolas Feybesse
 *
 */
public class ApplicationServer extends AbstractBackEnd<PersistantApplication> {

	public static void main(String[] args) {
		new ApplicationServer(new GSDeploymentOptions()).start();
	}

	public ApplicationServer(GSDeploymentOptions options) {
		super(options);
		if (options.getEngines().isEmpty()) {
			PersistantApplication defaultRoot = buildApp(TodoApp.class, null, options.getClasses());
			roots.put("/" + TodoApp.class.getSimpleName(), defaultRoot);
			System.out.println("Starts application : " + "/" + Statics.ENGINE_VALUE);
		} else
			for (Entry<String, String> entry : options.getEngines().entrySet()) {
				PersistantApplication root = buildApp(TodoApp.class, entry.getValue(), options.getClasses());
				roots.put("/" + entry.getKey(), root);
				System.out.println("Starts application : " + "/" + entry.getKey());
			}
	}

	protected PersistantApplication buildApp(Class<? extends HtmlApp> applicationClass, String persistentDirectoryPath, Class[] userClasses) {
		return new PersistantApplication(applicationClass, new Engine(persistentDirectoryPath, userClasses));
	}

	public class WebSocketsServer extends AbstractWebSocketsServer<PersistantApplication> {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(PersistantApplication application, ServerWebSocket socket) {
			Cache cache = application.getEngine().newCache();
			HtmlApp app = cache.safeSupply(() -> application.newHtmlApp(socket));
			return buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				String message = gsBuffer.getString(0, gsBuffer.length());
				JsonObject json = new JsonObject(message);
				HtmlDomNode node = app.getNodeById(json.getString(HtmlElement.ID));
				if (node != null)
					cache.safeConsum((x) -> node.handleMessage(json));
			};
		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(GSDeploymentOptions options) {
		return new WebSocketsServer(options.getHost(), options.getPort());
	}
}
