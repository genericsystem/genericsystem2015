package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import com.sun.xml.internal.ws.api.pipe.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.MessageConsumer;

/**
 * The OcrVerticle receives a message from the event bus when an image has been successfully de-zoned. The image is then processed (e.g., transformed and OCR'd) and the results from the OCR are stored in Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class OcrVerticle extends AbstractVerticle {

	private Root engine;

	public static void main(String[] args) {
		VertxOptions options = new VertxOptions().setMaxWorkerExecuteTime(Long.MAX_VALUE);
		Vertx vertx = Vertx.vertx(options);
		OcrVerticle ocrVerticle = new OcrVerticle(FillModelWithData.getEngine());
		vertx.deployVerticle(ocrVerticle, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticles failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
		deployTestVerticle();
	}

	/**
	 * Default constructor. A reference to an {@link Engine} must be provided to be able to save the results.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public OcrVerticle(Root engine) {
		this.engine = engine;
	}

	/**
	 * Test method to deploy a test Verticle that will send a message to the OCR Verticle.
	 * <p>
	 * <b>This method should only be used for testing purposes.</b>
	 */
	public static void deployTestVerticle() {
		AbstractVerticle testVerticle = new AbstractVerticle() {
			@Override
			public void start() throws Exception {
				Path imagePath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image2-0.png");
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath.toString());
			}
		};
		VerticleDeployerFromWatchApp.deployWorkerVerticle(testVerticle, "Deployment of verticles failed.");
	}

	// TODO: refactor the code, since the file is now stored before being sent to OCR (and thus, is always known)
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
