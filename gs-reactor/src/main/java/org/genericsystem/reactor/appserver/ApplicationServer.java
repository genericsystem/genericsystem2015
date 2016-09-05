package org.genericsystem.reactor.appserver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.common.Root;
import org.genericsystem.kernel.Cache;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.appserver.WebAppsConfig.SimpleWebAppConfig;
import org.genericsystem.reactor.gs.GSApp;

import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 */

public class ApplicationServer extends AbstractBackEnd {
	protected static Logger log = LoggerFactory.getLogger(ApplicationServer.class);
	protected Map<String, PersistentApplication> apps = new HashMap<>();

	public ApplicationServer(WebAppsConfig options) {
		super(options.getHost(), options.getPort());
		log.info("Load config : \n" + options.encodePrettily());
		for (String directoryPath : options.getPersistentDirectoryPaths()) {
			String path = directoryPath != null ? directoryPath : "/";
			Root root = buildRoot(directoryPath, options.getClasses(directoryPath), options.getEngineClass(directoryPath));
			log.info("Starts " + root.getClass().getSimpleName() + " with path : " + path + " and persistence directory path : " + directoryPath);
			if (directoryPath == null)
				directoryPath = "/";
			roots.put(path, root);
		}
		for (String applicationPath : options.getApplicationsPaths()) {
			String directoryPath = options.getPersistentDirectoryPath(applicationPath);
			String path = directoryPath != null ? directoryPath : "/";
			apps.put(applicationPath, new PersistentApplication(options.getApplicationClass(applicationPath), options.getModelClass(applicationPath), roots.get(path), options.getRootId()));

		}
	}

	public static void startSimpleGenericApp(String[] mainArgs, Class<? extends GSApp> htmlAppClass, String homePersistentDirectoryPath) {
		new ApplicationServer(new SimpleWebAppConfig(mainArgs, htmlAppClass, homePersistentDirectoryPath)).start();
	}

	protected Root buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses, Class<? extends Root> applicationClass) {
		try {
			return applicationClass.getConstructor(String.class, Class[].class).newInstance(persistentDirectoryPath, userClasses.toArray(new Class[userClasses.size()]));
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new IllegalStateException(e);
		}
	}

	// protected PersistentApplication buildApp(Class<? extends TagTree> applicationClass, String persistentDirectoryPath, List<Class<?>> userClasses, Class<? extends Model> modelClass, Root engine, String rootId) {
	// return new PersistentApplication(applicationClass, modelClass, engine, rootId);
	// }

	private class WebSocketsServer extends AbstractWebSocketsServer {

		public void cachesShiftTs() {
			for (Cache cache : caches.values()) {
				cache.shiftTs();
			}
		}

		private Map<ServerWebSocket, Cache> caches = new ConcurrentHashMap<ServerWebSocket, Cache>();

		public WebSocketsServer(String host, int port) {
			super(host, port);
			// GSVertx.vertx().getVertx().setPeriodic(5000, l -> cachesShiftTs());
		}

		@Override
		public Handler<Buffer> getHandler(String path, ServerWebSocket socket) {

			PersistentApplication application = apps.get(path);
			if (application == null) {
				throw new IllegalStateException("Unable to load an application with path : " + path);
			}
			Cache cache = (Cache) application.getEngine().newCache();
			RootHtmlDomNode rootHtmlDomNode = cache.safeSupply(() -> application.init(socket));
			caches.put(socket, cache);
			// log.info("Open new socket : " + socket);
			return buffer -> {
				// log.info("Receive new message for socket : " + socket);
				GSBuffer gsBuffer = new GSBuffer(buffer);
				String message = gsBuffer.getString(0, gsBuffer.length());
				JsonObject json = new JsonObject(message);
				HtmlDomNode node = rootHtmlDomNode.getNodeById(json.getString(HtmlDomNode.ID));
				if (node != null) {
					cache.safeConsum((x) -> node.handleMessage(json));
				} else
					log.info("Can't find node id : " + json.getString(HtmlDomNode.ID));
			};
		}

		@Override
		public Handler<Void> getCloseHandler(ServerWebSocket socket) {
			return (v) -> {
				System.out.println("Close socket");
				caches.remove(socket);
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
				// log.info("Request received with application path : " + appPath);
				PersistentApplication application = apps.get("/" + appPath);
				if (application == null) {
					request.response().end("No application is configured with path : /" + appPath);
					log.info("No application is configured with path : /" + appPath);
					return;
				}
				// log.info("Request detected for application : " + application.getApplicationClass().getName());
				int shift = appPath.isEmpty() ? 0 : 1;
				shift += items.length > shift ? 1 : 0;
				String resourceToServe = request.path().substring(appPath.length() + shift);
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
					indexHtml += "<body onload=\"connect();\" id=\"" + application.getRootId() + "\">";
					indexHtml += "</body>";
					indexHtml += "</html>";
					request.response().end(indexHtml);
				} else {
					InputStream input = application.getApplicationClass().getResourceAsStream("/" + resourceToServe);
					if (input == null) {
						if (resourceToServe.endsWith(".css")) {
							log.warn("Unable to find resource : /" + resourceToServe + ", get the reactor standard reactor.css instead");
							input = ApplicationServer.class.getResourceAsStream("/reactor.css");
						} else if (resourceToServe.endsWith(".js")) {
							log.warn("Unable to find resource : /" + resourceToServe + ", get the reactor standard script.js instead");
							input = ApplicationServer.class.getResourceAsStream("/script.js");
						} else if (resourceToServe.endsWith("favicon.ico")) {
							log.warn("Unable to find resource : /" + resourceToServe + ", get the reactor standard favicon.ico instead");
							input = ApplicationServer.class.getResourceAsStream("/favicon.ico");
						} else if (resourceToServe.endsWith(".ico") || resourceToServe.endsWith(".jpg") || resourceToServe.endsWith(".png")) {
							log.warn("Unable to find resource : /" + resourceToServe + ", get nothing instead");
							request.response().end();
							return;
						} else
							throw new IllegalStateException("Unable to find resource : " + resourceToServe);
					}
					if (resourceToServe.endsWith(".ico")) {
						MultiMap headers = request.response().headers();
						Buffer buffer = makeBuffer(input);
						headers.add(HttpHeaders.CONTENT_TYPE, "image/x-icon");
						headers.add(HttpHeaders.CONTENT_LENGTH, Integer.toString(buffer.length()));
						headers.add(HttpHeaders.CACHE_CONTROL, "public, max-age=" + 86400);
						request.response().end(buffer);
					} else if (resourceToServe.endsWith(".jpg") || resourceToServe.endsWith(".png")) {
						// TODO
						MultiMap headers = request.response().headers();
						Buffer buffer = makeBuffer(input);
						// headers.add(HttpHeaders.CONTENT_TYPE, "image/x-icon");
						headers.add(HttpHeaders.CONTENT_LENGTH, Integer.toString(buffer.length()));
						headers.add(HttpHeaders.CACHE_CONTROL, "public, max-age=" + 86400);
						request.response().end(buffer);
					} else {
						String result = new BufferedReader(new InputStreamReader(input)).lines().collect(Collectors.joining("\n"));
						request.response().end(result);
					}
				}
			});
		}

		private Buffer makeBuffer(InputStream input) {
			Buffer buffer = Buffer.buffer();
			int read;
			byte[] data = new byte[4096];
			try {
				while ((read = input.read(data, 0, data.length)) != -1) {
					if (read == data.length) {
						buffer.appendBytes(data);
					} else {
						byte[] slice = new byte[read];
						System.arraycopy(data, 0, slice, 0, slice.length);
						buffer.appendBytes(slice);
					}
				}
			} catch (IOException e) {
				throw new IllegalStateException(e);
			}
			return buffer;
		}

		@Override
		public void start() {
			super.start();
			String logo = "\n";
			logo += ("____________________________________________________________________________________________________________\n");
			logo += ("|___________________________________________________________________________________________________________|\n");
			logo += ("|___________________________________________________________________________________________________________|\n");
			logo += ("|____________|         ____                      _      ____             __                  /______________|\n");
			logo += ("|____________|        / ___)___  _  _____  ___  /_)__  / ___)_  __ ___  / /  ___  ____      /_______________|\n");
			logo += ("|____________|       / /___/ __)/ \\/ / __)/ _ )/ |/ _)/___ \\/ \\/  ) __)/___)/ __)/    )    /________________|\n");
			logo += ("|____________|      / /_  / __)/    / __)/   \\/  / /_ ___/ /\\    (__  / /_ / __)/ / / /   /_________________|\n");
			logo += ("|____________|      \\____(____(_/\\_(____(_/\\_(__(____(____/  \\  (____(____(____(_/_/_/   /__________________|\n");
			logo += ("|____________|                                               /_/                        /___________________|\n");
			logo += ("|____________|_________________________________________________________________________/____________________|\n");
			logo += ("|___________________________________________________________________________________________________________|\n");
			logo += ("|___________________________________________________________________________________________________________|  \n");

			log.info(logo);
			log.info("Generic System Server is ready!");
		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}
}
