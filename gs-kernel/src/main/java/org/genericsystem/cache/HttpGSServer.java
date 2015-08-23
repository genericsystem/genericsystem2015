package org.genericsystem.cache;

import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;

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

	public static class GsDeploymentConfig extends JsonObject {

		public GsDeploymentConfig() {
			super.put("host", Statics.DEFAULT_HOST);
			super.put("port", Statics.DEFAULT_PORT);
			super.put("engines", new JsonArray());
			addEngine(null, null);
		}

		public GsDeploymentConfig setHost(String host) {
			super.put("host", host);
			return this;
		}

		public GsDeploymentConfig setPort(int port) {
			super.put("port", port);
			return this;
		}

		public GsDeploymentConfig addEngine(String engineValue, String repositoryPath) {
			super.getJsonArray("engines").add(new JsonObject().put("engineValue", engineValue).put("engineRepositoryPath", repositoryPath));
			return this;
		}
	}
}
