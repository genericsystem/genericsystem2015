package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.DidjvuScript;
import org.genericsystem.cv.Ocr;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new OcrVerticle());
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// Added a simple Verticle that will publish an example string on the event bus
		VerticleDeployer.deployVerticle(new AbstractVerticle() {
			@Override
			public void start() throws Exception {
				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image-1.png");
				vertx.eventBus().publish(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS, imagePath.toString());
			}
		});
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);
			Ocr.ocrNewClassifiedImg(Paths.get(imagePath));
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
