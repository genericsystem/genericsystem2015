package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	public static void main(String[] args) {
		VertxOptions options = new VertxOptions().setMaxWorkerExecuteTime(Long.MAX_VALUE);
		
//		VerticleDeployer.deployVerticle(new OcrVerticle(), options);
		Vertx vertx = Vertx.vertx(options);
		vertx.deployVerticle(new OcrVerticle(), res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
		
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// Added a simple Verticle that will publish an example string on the event bus
//		VerticleDeployer.deployVerticle(new AbstractVerticle() {
//			@Override
//			public void start() throws Exception {
//				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image5-0.png");
//				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath.toString());
//			}
//		});
		vertx.deployVerticle(new AbstractVerticle() {
			@Override
			public void start() throws Exception {
				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image5-0.png");
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath.toString());
				}
			} , res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.ACCURATE_ZONES_FOUND);
		// TODO: use a worker thread?
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);
			
			int result = Ocr.ocrNewClassifiedImg(Paths.get(imagePath));
			switch (result) {
			case FillModelWithData.NEW_FILE:
				System.out.println("New image (processed)");
				vertx.eventBus().publish(VerticleDeployer.NEW_IMAGE_PROCESSED, imagePath);
				future.complete();
				break;
			case FillModelWithData.KNOWN_FILE:
				System.out.println("Known image (passed)");
				vertx.eventBus().publish(VerticleDeployer.KNOWN_IMAGE_PASSED, imagePath);
				future.complete();
				break;
			case FillModelWithData.KNOWN_FILE_UPDATED_FILTERS:
				System.out.println("Known image (updated)");
				vertx.eventBus().publish(VerticleDeployer.KNOWN_IMAGE_PROCESSED, imagePath);
				future.complete();
				break;
			default:
				future.fail("Unhandled case!");
				break;
			}
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
