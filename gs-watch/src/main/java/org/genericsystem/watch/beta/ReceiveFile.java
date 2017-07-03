package org.genericsystem.watch.beta;

import java.io.File;
import java.io.FileOutputStream;

import io.vertx.core.AbstractVerticle;

public class ReceiveFile extends AbstractVerticle {

	public static void main(String[] args) {
		Runner.runExample(ReceiveFile.class);
	}

	@Override
	public void start() throws Exception {

		vertx.createHttpClient().getNow(8080, "localhost", "/image-0.png", resp -> {
			System.out.println("Got response " + resp.statusCode());
			resp.bodyHandler(body -> {
				FileOutputStream fos;
				try {
					fos = new FileOutputStream(new File("image-1.png"));
					fos.write(body.getBytes());
					fos.close();
				} catch (Exception e) {
					e.printStackTrace();
				}
				System.out.println("Ok");
			});

			// vertx.fileSystem().open("/home/middleware/git/genericsystem2015/gs-watch/image-1.png",
			// new OpenOptions(), ares -> {
			// if (ares.failed()) {
			// System.out.println(ares.cause());
			// return;
			// }
			//
			// AsyncFile file = ares.result();
			// Pump pump = Pump.pump(resp, file);
			// resp.endHandler(v1 -> file.close(v2 -> {
			// System.out.println("Downloaded to " + "image-1.png");
			// }));
			// pump.start();
			// });
			//
		});
	}
}
