package org.genericsystem.watch;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;

public class App {
	public static void main(String[] args) {
		VertxOptions options = new VertxOptions();
		options.setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx vertx = Vertx.vertx(options);
		vertx.deployVerticle(new VerticleDeployer(), res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			System.out.println("Deployment finished");
		});
	}
}
