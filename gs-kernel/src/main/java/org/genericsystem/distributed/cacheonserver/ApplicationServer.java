package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.genericsystem.common.AbstractBackEnd;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractWebSocketsServer;
import org.genericsystem.common.GSBuffer;
import org.genericsystem.distributed.ApplicationsDeploymentConfig;
import org.genericsystem.distributed.cacheonserver.ui.exemple.AppHtml;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Car;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.CarColor;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Color;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.HtmlDomNode;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
public class ApplicationServer extends AbstractBackEnd {

	public static void main(String[] args) {
		ApplicationsDeploymentConfig apps = new ApplicationsDeploymentConfig();
		apps.addApplication("/", AppHtml.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class, Power.class,
				Color.class, CarColor.class);
		apps.addApplication("/second", AppHtml.class, "/home/middleware/cars/", Car.class, Power.class, Color.class,
				CarColor.class);
		// apps.addApplication("/todos", TodoApp.class, "/home/middleware/todos/", Todos.class);
		new ApplicationServer(apps).start();
	}

	protected Map<String, PersistentApplication> apps = new HashMap<>();

	public ApplicationServer(ApplicationsDeploymentConfig options) {
		super(options.getHost(), options.getPort());
		try {
			getIPAddress();
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// System.out.println("Load IP : " + getLocalIp() + "\n");
		System.out.println("Load config : \n" + options.encodePrettily());
		for (String directoryPath : options.getPersistentDirectoryPaths()) {
			String path = directoryPath != null ? directoryPath : "/";
			AbstractServer root = buildRoot(directoryPath, options.getClasses(directoryPath));
			System.out.println("Starts engine with path : " + path + " and persistence directory path : "
					+ directoryPath);
			if (directoryPath == null)
				directoryPath = "/";
			roots.put(path, root);
		}
		for (String applicationPath : options.getApplicationsPaths()) {
			String directoryPath = options.getPersistentDirectoryPath(applicationPath);
			String path = directoryPath != null ? directoryPath : "/";
			apps.put(applicationPath,
					new PersistentApplication(options.getApplicationClass(applicationPath), roots.get(path)));
			System.out.println("Starts application : " + options.getApplicationClass(applicationPath).getSimpleName()
					+ " with path : " + applicationPath + " and persistence directory path : " + directoryPath);
		}
	}

	protected AbstractServer buildRoot(String persistentDirectoryPath, Set<Class<?>> userClasses) {
		return new Engine(persistentDirectoryPath, userClasses.stream().toArray(Class[]::new));
	}

	protected PersistentApplication buildApp(Class<? extends AppHtml> applicationClass, String persistentDirectoryPath,
			List<Class<?>> userClasses) {
		return new PersistentApplication(applicationClass, new Engine(persistentDirectoryPath, userClasses.stream()
				.toArray(Class[]::new)));
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
				HtmlDomNode node = ((AppHtml) app).getNodeById(json.getString(HtmlElement.ID));
				if (node != null)
					cache.safeConsum((x) -> node.handleMessage(json));
			};
		}
	}

	@Override
	protected WebSocketsServer buildWebSocketsServer(String host, int port) {
		return new WebSocketsServer(host, port);
	}

	public String getLocalIp() {
		String hostAdr = null;
		try {
			InetAddress thisIp = InetAddress.getLocalHost();
			// System.out.println("IP:" + thisIp.getHostAddress());
			hostAdr = thisIp.getHostAddress();
		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
		return hostAdr;
	}

	public void getIPAddress() throws SocketException {
		char[] s = null;
		Enumeration e = NetworkInterface.getNetworkInterfaces();
		while (e.hasMoreElements()) {
			NetworkInterface n = (NetworkInterface) e.nextElement();
			Enumeration ee = n.getInetAddresses();
			while (ee.hasMoreElements()) {
				InetAddress i = (InetAddress) ee.nextElement();
				s = i.getHostAddress().toCharArray();
				System.out.println(s);
				// System.out.println(s[0] + "." + s[1] + "." + s[2]);
			}
		}
	}
}
