package org.genericsystem.reactor.appserver;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Element.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlApp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */

public class ApplicationServer extends AbstractBackEnd {
	protected static Logger log = LoggerFactory.getLogger(ApplicationServer.class);
	protected Map<String, PersistentApplication> apps = new HashMap<>();

	public ApplicationServer(ApplicationsDeploymentConfig options) {
		super(options.getHost(), options.getPort());
		log.info("Load config : \n" + options.encodePrettily());
		for (String directoryPath : options.getPersistentDirectoryPaths()) {
			String path = directoryPath != null ? directoryPath : "/";
			AbstractRoot root = buildRoot(directoryPath, options.getClasses(directoryPath), options.getEngineClass(directoryPath));
			log.info("Starts " + root.getClass().getSimpleName() + " with path : " + path + " and persistence directory path : " + directoryPath);
			if (directoryPath == null)
				directoryPath = "/";
			roots.put(path, root);
		}
		for (String applicationPath : options.getApplicationsPaths()) {
			String directoryPath = options.getPersistentDirectoryPath(applicationPath);
			String path = directoryPath != null ? directoryPath : "/";
			apps.put(applicationPath,
					new PersistentApplication(options.getApplicationClass(applicationPath), options.getModelClass(applicationPath), roots.get(path)));

		}
	}

	protected AbstractRoot buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses, Class<? extends AbstractRoot> applicationClass) {
		try {
			return applicationClass.getConstructor(String.class, Class[].class).newInstance(persistentDirectoryPath,
					userClasses.toArray(new Class[userClasses.size()]));
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException
				| InvocationTargetException e) {
			throw new IllegalStateException(e);
		}
	}

	protected PersistentApplication buildApp(Class<? extends HtmlApp<?>> applicationClass, String persistentDirectoryPath, List<Class<?>> userClasses,
			Class<? extends Model> modelClass, AbstractRoot engine) {
		return new PersistentApplication(applicationClass, modelClass, engine);
	}

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {
			PersistentApplication application = apps.get(path);
			if (application == null)
				throw new IllegalStateException("Unable to load an application with path : " + path);
			AbstractCache cache = application.getEngine().newCache();
			Element app = cache.safeSupply(() -> application.newHtmlApp(socket));
			return buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				String message = gsBuffer.getString(0, gsBuffer.length());
				JsonObject json = new JsonObject(message);
				HtmlDomNode node = ((HtmlApp) app).getNodeById(json.getString(Element.ID));
				if (node != null)
					cache.safeConsum((x) -> node.handleMessage(json));
			};
		}

		@Override
		public void addHttpHandler(HttpServer httpServer, String url) {
			httpServer.requestHandler(request -> {
				String[] items = request.path().split("/");
				String appPath = "";

				if (items.length > 1) {
					appPath += items[1].trim();
				}

				PersistentApplication application = apps.get("/" + appPath);
				if (application == null) {
					request.response().end();
					throw new IllegalStateException("Unable to load an application with path : " + appPath);
				}

				if (request.path().contains(".css")) {
					request.response().sendFile(application.getApplicationClass().getClassLoader().getResource(appPath + ".css").getFile());
				} else if (request.path().contains(".js")) {
					request.response().sendFile(getClass().getClassLoader().getResource("script.js").getFile());
				} else {
					String indexHtml = "<!DOCTYPE html>";
					indexHtml += "<html>";
					indexHtml += "<head>";
					indexHtml += "<meta charset=\"UTF-8\">";
					indexHtml += "<LINK rel=stylesheet type=\"text/css\" href=\"/" + appPath + "/resources/style.css\"/>";
					indexHtml += "<script>";
					indexHtml += "var serviceLocation =\"" + url + request.path() + "\";";
					indexHtml += "</script>";
					indexHtml += "<script type=\"text/javascript\" src=\"/" + appPath + "/javascript/script.js\"></script>";
					indexHtml += "</head>";
					indexHtml += "<body onload=\"connect();\" id=\"root\">";
					indexHtml += "</body>";
					indexHtml += "</html>";
					request.response().end(indexHtml);
				}
			});

		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
