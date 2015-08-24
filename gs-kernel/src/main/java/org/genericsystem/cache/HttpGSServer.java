package org.genericsystem.cache;

import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;

import org.genericsystem.kernel.Root;

public class HttpGSServer extends AbstractGSServer {
	@Override
	public void start() {
		super.start();
		HttpServer httpServer = vertx.createHttpServer(new HttpServerOptions().setPort(config().getInteger("port")));
		httpServer.requestHandler(request -> {
			String path = request.path();
			Root root = getRoots().get(path);
			if (root == null)
				throw new IllegalStateException("Unable to find database :" + path);
			request.exceptionHandler(e -> {
				e.printStackTrace();
			});
			request.handler(getHandler(root, buffer -> {
				request.response().end(buffer);
				request.response().close();
			}));
		});
		httpServer.listen();
		System.out.println("Generic System server ready!");
	}

	@Override
	public void stop() {
		super.stop();
		System.out.println("Generic System server stopped!");
	}
}
