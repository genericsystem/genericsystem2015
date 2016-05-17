package org.genericsystem.reactor.appserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlApp;

/**
 * @author Nicolas Feybesse
 *
 */
public class ApplicationServer extends AbstractBackEnd {

	protected Map<String, PersistentApplication> apps = new HashMap<>();

	public ApplicationServer(ApplicationsDeploymentConfig options) {
		super(options.getHost(), options.getPort());
		System.out.println("Load config : \n" + options.encodePrettily());
		for (String directoryPath : options.getPersistentDirectoryPaths()) {
			String path = directoryPath != null ? directoryPath : "/";
			AbstractRoot root = buildRoot(directoryPath, options.getClasses(directoryPath), options.getApplicationClass());
			System.out.println("Starts engine with path : " + path + " and persistence directory path : "
					+ directoryPath);
			if (directoryPath == null)
				directoryPath = "/";
			roots.put(path, root);
		}
		for (String applicationPath : options.getApplicationsPaths()) {
			String directoryPath = options.getPersistentDirectoryPath(applicationPath);
			String path = directoryPath != null ? directoryPath : "/";
			apps.put(
					applicationPath,
					new PersistentApplication(options.getApplicationClass(applicationPath), options
							.getModelClass(applicationPath), roots.get(path)));
			System.out.println("Starts application : " + options.getApplicationClass(applicationPath).getSimpleName()
					+ " with path : " + applicationPath + " and persistence directory path : " + directoryPath);
		}
	}

	protected AbstractRoot buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses,
			Class<? extends AbstractRoot> applicationClass) {

		Constructor constructeur = null;
		try {
			constructeur = applicationClass.getConstructor(String.class, Class[].class);
			return (AbstractRoot) constructeur.newInstance(persistentDirectoryPath,
					userClasses.toArray(new Class[userClasses.size()]));
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
				| IllegalArgumentException | InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return null;

	}

	protected PersistentApplication buildApp(Class<? extends HtmlApp<?>> applicationClass,
			String persistentDirectoryPath, List<Class<?>> userClasses, Class<? extends Model> modelClass,
			AbstractRoot engine) {
		return new PersistentApplication(applicationClass, modelClass, engine);
	}

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public WebSocketsServer(String host, int port) {
			super(host, port);
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {
			System.out.println("socket path: " + path);
			System.out.println("Size apps : " + apps.size());
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

		@Override
		public void addHttpHandler(HttpServer httpServer, String url) {
			httpServer.requestHandler(request -> {
				// TODO
					String path = request.path();
					request.handler(getHttpHandler(path, request, url));
					request.exceptionHandler(e -> {
						e.printStackTrace();
						throw new IllegalStateException(e);
					});
				});
		}

		public Handler<Buffer> getHttpHandler(String path, HttpServerRequest request, String url) {
			System.out.println("IN PATH : " + path);
			System.out.println("IN request : " + request.toString());
			System.out.println("IN url : " + url);
			PersistentApplication application = apps.get(path);
			// if (application == null)
			// throw new IllegalStateException("Unable to load an application with path : " + path);

			String[] items = request.path().split("/");
			if ((items.length > 1) && ("resources".equals(items[1]))) {
				request.response().sendFile(Paths.get("").toAbsolutePath().toString() + request.path());
			} else {
				if (application == null)
					throw new IllegalStateException("Unable to load an application with path : " + path);

				String nameClass = application.getApplicationClass().getSimpleName();

				String indexHtml = "<!DOCTYPE html>";
				indexHtml += "<html>";
				indexHtml += "<head>";
				indexHtml += "<meta charset=\"UTF-8\">";
				indexHtml += "<script src=\"http://code.jquery.com/jquery-2.2.0.min.js\"></script>";
				indexHtml += "<LINK rel=stylesheet type=\"text/css\" href=\"resources/" + nameClass + ".css\"/>";
				indexHtml += "<script>";
				indexHtml += "var serviceLocation =\"" + url + request.path() + "\";";
				indexHtml += "</script>";
				indexHtml += "<script type=\"text/javascript\" src=\"resources/script.js\"></script>";
				indexHtml += "</head>";
				indexHtml += "<body id=\"root\">";
				indexHtml += "</body>";
				indexHtml += "</html>";
				request.response().end(indexHtml);

			}

			return null;
		}

	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
