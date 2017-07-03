package org.genericsystem.watch.beta;

import io.vertx.core.AbstractVerticle;

public class SendFile extends AbstractVerticle {

	public static void main(String[] args) {
		Runner.runExample(SendFile.class);
	}

	@Override
	public void start() throws Exception {
		vertx.createHttpServer().requestHandler(req -> {
			String fileName = req.path().replace("/", "");
			System.out.println("Will send :" + fileName);
			req.response().sendFile(fileName);
		}).listen(8080);
		//
	}
}