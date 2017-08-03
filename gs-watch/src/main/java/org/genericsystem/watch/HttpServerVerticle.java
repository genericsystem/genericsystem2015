package org.genericsystem.watch;

import java.lang.invoke.MethodHandles;
import java.net.BindException;
import java.net.SocketException;
import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class HttpServerVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] args) throws UnknownHostException, SocketException {
		Vertx.vertx().deployVerticle(new HttpServerVerticle());
	}

	@Override
	public void start(Future<Void> future) throws Exception {
		vertx.createHttpServer().requestHandler(req -> {
			String fileName = req.path();
			String path = DistributedVerticle.BASE_PATH + fileName;
			logger.debug("Will send file {}.", path);
			req.response().sendFile(path);
		}).listen(8084, ar -> {
			if (ar.failed()) {
				if (ar.cause() instanceof BindException) {
					logger.warn("BindException ignored, HttpServer already started.");
					future.complete();
				} else
					future.fail(ar.cause());
			} else
				future.complete();
		});
	}

}
