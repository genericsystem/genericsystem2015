package org.genericsystem.watch;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class AddImageToEngineVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(AddImageToEngineVerticle.class);
	private Root engine;

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new AddImageToEngineVerticle());
	}

	public AddImageToEngineVerticle() {

	}

	public AddImageToEngineVerticle(Root engine) {
		this.engine = engine;
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			System.out.println(">>> add image: " + Thread.currentThread().getName());

			String imagePath = message.body();
			System.out.println(">>>>> New image to register: " + imagePath);
			boolean result;
			if (null != engine)
				result = FillModelWithData.registerNewFile(engine, Paths.get(imagePath));
			else
				result = FillModelWithData.registerNewFile(Paths.get(imagePath));
			if (result) {
				vertx.eventBus().publish(VerticleDeployer.NEW_IMAGE_ADDED_TO_CLASS, imagePath);
				future.complete();
			} else
				future.fail("An error has occured while saving file " + imagePath);
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
