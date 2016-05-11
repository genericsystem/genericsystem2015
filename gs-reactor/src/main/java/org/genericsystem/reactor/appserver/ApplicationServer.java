package org.genericsystem.reactor.appserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.html.HtmlApp;

/**
 * @author Nicolas Feybesse
 *
 */
public class ApplicationServer extends AbstractBackEnd {

	// TODO move example
	// public static void main(String[] args) {
	// ApplicationsDeploymentConfig apps = new ApplicationsDeploymentConfig();
	// apps.addApplication("/", AppHtml.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class, Power.class, Color.class, CarColor.class);
	// apps.addApplication("/second", AppHtml.class, "/home/middleware/cars/", Car.class, Power.class, Color.class, CarColor.class);
	// // apps.addApplication("/todos", TodoApp.class, "/home/middleware/todos/", Todos.class);
	// new ApplicationServer(apps).start();
	// }

	protected Map<String, PersistentApplication> apps = new HashMap<>();

	public ApplicationServer(ApplicationsDeploymentConfig options) {
		super(options.getHost(), options.getPort());
		System.out.println("Load config : \n" + options.encodePrettily());
		for (String directoryPath : options.getPersistentDirectoryPaths()) {
			String path = directoryPath != null ? directoryPath : "/";
			AbstractRoot root = buildRoot(directoryPath, options.getClasses(directoryPath), options.getEngineBuilder(applicationPath));
			System.out.println("Starts engine with path : " + path + " and persistence directory path : " + directoryPath);
			if (directoryPath == null)
				directoryPath = "/";
			roots.put(path, root);
		}
		for (String applicationPath : options.getApplicationsPaths()) {
			String directoryPath = options.getPersistentDirectoryPath(applicationPath);
			String path = directoryPath != null ? directoryPath : "/";
			apps.put(applicationPath, new PersistentApplication(options.getApplicationClass(applicationPath), roots.get(path)));
			System.out.println("Starts application : " + options.getApplicationClass(applicationPath).getSimpleName() + " with path : " + applicationPath
					+ " and persistence directory path : " + directoryPath);
		}
	}

	protected AbstractRoot buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses,
			BiFunction<String, Class<? extends Generic>[], AbstractRoot> engineBuilder) {
		return engineBuilder.apply(persistentDirectoryPath, userClasses.stream().toArray(Class[]::new));
	}

	protected PersistentApplication buildApp(Class<? extends HtmlApp<?>> applicationClass, String persistentDirectoryPath, List<Class<?>> userClasses,
			BiFunction<String, Class<? extends Generic>[], AbstractRoot> engineBuilder) {
		return new PersistentApplication(applicationClass, engineBuilder.apply(persistentDirectoryPath, userClasses.stream().toArray(Class[]::new)));
	}

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {
			System.out.println("socket path: " + path);
			PersistentApplication application = apps.get(path);
			if (application == null)
				throw new IllegalStateException("Unable to load an application with path : " + path);
			AbstractCache cache = application.getEngine().newCache();
			HtmlElement app = cache.safeSupply(() -> application.newHtmlApp(socket));
			return buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				String message = gsBuffer.getString(0, gsBuffer.length());
				JsonObject json = new JsonObject(message);
				HtmlDomNode node = ((HtmlApp) app).getNodeById(json.getString(HtmlElement.ID));
				if (node != null)
					cache.safeConsum((x) -> node.handleMessage(json));
			};
		}

	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
