package org.genericsystem.watch;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import com.sun.xml.internal.ws.api.pipe.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

/**
 * The AddImageToEngineVerticle receives a message from the event bus when an image was classified. After saving the document in Generic System, a message is sent to the {@link DezonerVerticle}. A reference to an {@link Engine} must be givent to be able to
 * store the data in Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class AddImageToEngineVerticle extends AbstractVerticle {

	private Root engine;

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new AddImageToEngineVerticle(FillModelWithData.getEngine()));
	}

	/**
	 * Default constructor. A reference to the engine must be provided.
	 * 
	 * @param engine - the engine used to store the data
	 */
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
