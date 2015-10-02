package org.genericsystem.distributed;

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import java.util.ArrayList;
import java.util.List;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.LightServerEngine;

public class HttpGSLightServer extends AbstractLightGSServer {

	private List<HttpServer> httpServers = new ArrayList<>();
	private final int port;
	private final String host;

	public HttpGSLightServer(GSDeploymentOptions options) {
		super(options);
		this.port = options.getPort();
		this.host = options.getHost();
	}

	public static void main(String[] args) {
		new HttpGSLightServer(new GSDeploymentOptions()).start();
	}

	@Override
	public void start() {
		Vertx vertx = GSVertx.vertx().getVertx();
		for (int i = 0; i < 2 * Runtime.getRuntime().availableProcessors(); i++) {
			HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(port).setHost(host));
			httpServer.requestHandler(request -> {
				String path = request.path();
				AbstractServer root = roots.get(path);
				if (root == null)
					throw new IllegalStateException("Unable to find database :" + path);
				request.exceptionHandler(e -> {
					e.printStackTrace();
					throw new IllegalStateException(e);
				});
				request.handler(buffer -> {
					GSBuffer gsBuffer = new GSBuffer(buffer);
					int methodId = gsBuffer.getInt();
					request.response().end(getReplyBuffer(methodId, (LightServerEngine) root, gsBuffer));
					request.response().close();
				});
			});
			AbstractGSServer.<HttpServer> synchonizeTask(handler -> httpServer.listen(handler));
			httpServers.add(httpServer);
		}
		System.out.println("Generic System server ready!");
	}

	@Override
	public void stop() {
		httpServers.forEach(httpServer -> AbstractGSServer.<Void> synchonizeTask(handler -> httpServer.close(handler)));
		super.stop();
		System.out.println("Generic System server stopped!");
	}

	@Override
	protected AbstractServer buildRoot(String value, String persistentDirectoryPath, Class<?>[] userClasses) {
		return new LightServerEngine(value, persistentDirectoryPath, userClasses);
	}
}
