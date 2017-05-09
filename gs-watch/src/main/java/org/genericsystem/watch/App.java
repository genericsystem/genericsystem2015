package org.genericsystem.watch;

import io.vertx.core.Vertx;

public class App {
	public static void main(String[] args) {
		Vertx vertx = Vertx.vertx();
		vertx.deployVerticle(new VerticleDeployer(), res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
		});
	}
}
