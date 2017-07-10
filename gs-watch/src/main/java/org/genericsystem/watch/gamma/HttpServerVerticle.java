package org.genericsystem.watch.gamma;

import java.net.SocketException;
import java.net.UnknownHostException;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;

public class HttpServerVerticle extends AbstractVerticle {

	public static void main(String[] args) throws UnknownHostException, SocketException {
		Vertx.vertx().deployVerticle(new HttpServerVerticle());
	}

	@Override
	public void start() throws Exception {
		vertx.createHttpServer().requestHandler(req -> {
			String fileName = req.path();
			String path = DistributedVerticle.BASE_PATH + fileName;
			System.out.println("Will send :" + path + " on thread " + Thread.currentThread());
			req.response().sendFile(path);
		}).listen(8084);
	}

}
