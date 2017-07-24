package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.common.GSVertx;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(OcrVerticle.class);
	private Root engine;

	public static void main(String[] args) {
		VertxOptions options = new VertxOptions().setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx vertx = Vertx.vertx(options);
		OcrVerticle ocrVerticle = new OcrVerticle();
		vertx.deployVerticle(ocrVerticle, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
		deployTestVerticle();
	}

	public OcrVerticle() {

	}

	public OcrVerticle(Root engine) {
		this.engine = engine;
	}

	/**
	 * Deploy an OcrVerticle as a new worker. This Verticle is given a reference to the GS engine, which will be passed as an argument in the external method calls.
	 */
	public void deployOcrVerticle() {
		DeploymentOptions options = new DeploymentOptions().setWorker(true);
		GSVertx.vertx().getVertx().deployVerticle(this, options, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("OcrVerticle deployed");
		});
	}

	/**
	 * Test method to deploy a test Verticle that will send a message to the OCR Verticle.
	 * 
	 */
	public static void deployTestVerticle() {
		AbstractVerticle testVerticle = new AbstractVerticle() {
			@Override
			public void start() throws Exception {
				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image2-0.png");
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath.toString());
			}
		};
		// VerticleDeployer.deployVerticle(testVerticle);
		GSVertx.vertx().getVertx().deployVerticle(testVerticle, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Test verticle deployed");
		});
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.ACCURATE_ZONES_FOUND);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			System.out.println(">>> ocr: " + Thread.currentThread().getName());

			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);

			int result = FillModelWithData.ERROR;
			if (null != engine)
				result = FillModelWithData.doImgOcr(engine, Paths.get(imagePath));
			else
				result = FillModelWithData.doImgOcr(Paths.get(imagePath));

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
