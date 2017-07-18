package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);
	private Root engine;

	public static void main(String[] args) {
		VertxOptions options = new VertxOptions().setMaxWorkerExecuteTime(Long.MAX_VALUE);
		// deployOcrVerticle(options);
		deployTestVerticle(options);

	}

	public OcrVerticle() {
		// TODO Auto-generated constructor stub
		System.out.println(">>> OcrVerticle() called");
	}

	public OcrVerticle(Root engine) {
		this.engine = engine;
		System.out.println(">>> OcrVerticle(Root engine) called");
	}

	public void deployVerticle(VertxOptions options) {
		// Vertx vertx = Vertx.vertx(options);
		OcrVerticle ocrVerticle = new OcrVerticle(engine);
		// vertx.deployVerticle(ocrVerticle, res -> {
		// if (res.failed())
		// throw new IllegalStateException("Deployment of verticles failed.", res.cause());
		// else
		// System.out.println("Verticle deployed");
		// });
		VerticleDeployer.deployVerticle(ocrVerticle);
	}

	public static void deployOcrVerticle(VertxOptions options) {
		Vertx vertx = Vertx.vertx(options);
		OcrVerticle ocrVerticle = new OcrVerticle();
		vertx.deployVerticle(ocrVerticle, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
	}

	public static void deployTestVerticle(VertxOptions options) {
		Vertx vertx = Vertx.vertx(options);
		AbstractVerticle testVerticle = new AbstractVerticle() {
			@Override
			public void start() throws Exception {
				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image4-1.png");
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath.toString());
			}
		};

		// vertx.deployVerticle(testVerticle, res -> {
		// if (res.failed())
		// throw new IllegalStateException("Deployment of verticles failed.", res.cause());
		// else
		// System.out.println("Verticle deployed");
		// });
		VerticleDeployer.deployVerticle(testVerticle);
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.ACCURATE_ZONES_FOUND);
		// TODO: use a worker thread?
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);

			int result = FillModelWithData.ERROR;
			if (null != engine)
				result = Ocr.ocrNewClassifiedImg(engine, Paths.get(imagePath));
			else
				result = Ocr.ocrNewClassifiedImg(Paths.get(imagePath));

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
