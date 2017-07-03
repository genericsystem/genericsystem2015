package org.genericsystem.watch;

import java.nio.file.Paths;

import org.genericsystem.cv.Ocr;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new OcrVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_TO_OCR);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);
//			Ocr.ocrClassifiedImage(Paths.get(imagePath));
			Ocr.ocrNewClassifiedImg(Paths.get(imagePath));
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
