package org.genericsystem.reactor.appserver;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Tag.HtmlDomNode;
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
			apps.put(applicationPath, new PersistentApplication(options.getApplicationClass(applicationPath), options.getModelClass(applicationPath), roots.get(path)));

		}
	}

	protected AbstractRoot buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses, Class<? extends AbstractRoot> applicationClass) {
		try {
			return applicationClass.getConstructor(String.class, Class[].class).newInstance(persistentDirectoryPath, userClasses.toArray(new Class[userClasses.size()]));
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new IllegalStateException(e);
		}
	}

	protected PersistentApplication buildApp(Class<? extends HtmlApp<?>> applicationClass, String persistentDirectoryPath, List<Class<?>> userClasses, Class<? extends Model> modelClass, AbstractRoot engine) {
		return new PersistentApplication(applicationClass, modelClass, engine);
	}

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {
			PersistentApplication application = apps.get(path);
			if (application == null) {
				throw new IllegalStateException("Unable to load an application with path : " + path);
			}
			AbstractCache cache = application.getEngine().newCache();
			Tag app = cache.safeSupply(() -> application.newHtmlApp(socket));
			return buffer -> {
				GSBuffer gsBuffer = new GSBuffer(buffer);
				String message = gsBuffer.getString(0, gsBuffer.length());
				JsonObject json = new JsonObject(message);
				HtmlDomNode node = ((HtmlApp) app).getNodeById(json.getString(Tag.ID));
				if (node != null)
					cache.safeConsum((x) -> node.handleMessage(json));
			};
		}

		@Override
		public void addHttpHandler(HttpServer httpServer) {
			httpServer.requestHandler(request -> {
				// log.info("Request received with path : " + request.path());
				String[] items = request.path().substring(1).split("/");
				// log.info("Request received with splited items : " + Arrays.toString(items));
				String appPath = items.length == 0 ? "" : items[0];
				if (appPath.endsWith(".js") || appPath.endsWith(".css") || appPath.endsWith(".ico") || appPath.endsWith(".jpg") || appPath.endsWith(".png"))
					appPath = "";
				// log.info("Request received with application path : /" + appPath);
				PersistentApplication application = apps.get("/" + appPath);
				if (application == null) {
					request.response().end("No application is configured with path : /" + appPath);
					throw new IllegalStateException("Unable to load an application with path : /" + appPath);
				}
				// log.info("Request detected for application : " + application.getApplicationClass().getName());
				String resourceToServe = request.path().substring(appPath.length() + 1);
				// log.info("Resource to serve : " + resourceToServe);
				if ("".equals(resourceToServe)) {
					String indexHtml = "<!DOCTYPE html>";
					indexHtml += "<html>";
					indexHtml += "<head>";
					indexHtml += "<meta charset=\"UTF-8\">";
					indexHtml += "<LINK rel=stylesheet type=\"text/css\" href=\"" + (appPath.isEmpty() ? "" : ("/" + appPath)) + "/" + application.getApplicationClass().getSimpleName().toLowerCase() + ".css\"/>";
					indexHtml += "<script>";
					indexHtml += "var serviceLocation = \"ws://\" + document.location.host + \"" + request.path() + "\";";
					indexHtml += "</script>";
					indexHtml += "<script type=\"text/javascript\" src=\"" + (appPath.isEmpty() ? "" : ("/" + appPath)) + "/" + application.getApplicationClass().getSimpleName().toLowerCase() + ".js\"></script>";
					indexHtml += "</head>";
					indexHtml += "<body onload=\"connect();\" id=\"root\">";
					indexHtml += "</body>";
					indexHtml += "</html>";
					request.response().end(indexHtml);
				} else {
					InputStream input = application.getApplicationClass().getResourceAsStream(resourceToServe);
					if (input == null) {
						if (resourceToServe.endsWith(".css")) {
							log.warn("Unable to find resource : " + resourceToServe + ", get the reactor standard css instead");
							input = ApplicationServer.class.getResourceAsStream("/reactor.css");
						} else if (resourceToServe.endsWith(".js")) {
							log.warn("Unable to find resource : " + resourceToServe + ", get the reactor standard js instead");
							input = ApplicationServer.class.getResourceAsStream("/script.js");
						} else if (resourceToServe.endsWith(".ico") || resourceToServe.endsWith(".jpg") || resourceToServe.endsWith(".png")) {
							log.warn("Unable to find resource : " + resourceToServe + ", get nothing instead");
							request.response().end();
							return;
						} else
							throw new IllegalStateException("Unable to find resource : " + resourceToServe);
					}
					String result = new BufferedReader(new InputStreamReader(input)).lines().collect(Collectors.joining("\n"));
					request.response().end(result);
				}
			});
		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
