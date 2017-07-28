package org.genericsystem.watch;

import java.net.BindException;
import java.net.SocketException;
import java.net.UnknownHostException;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class HttpServerVerticle extends AbstractVerticle {

	public static void main(String[] args) throws UnknownHostException, SocketException {
		Vertx.vertx().deployVerticle(new HttpServerVerticle());
	}

	@Override
	public void start(Future<Void> future) throws Exception {
		vertx.createHttpServer().requestHandler(req -> {
			String fileName = req.path();
			String path = DistributedVerticle.BASE_PATH + fileName;
			System.out.println("Will send :" + path + " on thread " + Thread.currentThread());
			req.response().sendFile(path);
		}).listen(8084, ar -> {
			if (ar.failed()) {
				if (ar.cause() instanceof BindException) {
					System.out.println("BindException ignored, HttpServer already started.");
					future.complete();
				} else
					future.fail(ar.cause());
			} else
				future.complete();
		});
	}

}
