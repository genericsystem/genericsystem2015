package org.genericsystem.watch;

import org.genericsystem.cv.DidjvuScript;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class RunScriptVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new RunScriptVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>> New classified image: " + imagePath);
			int exitValue = DidjvuScript.runDidjvuScript(imagePath);
			if (exitValue == 0) {
				vertx.eventBus().publish(VerticleDeployer.IMAGE_TO_OCR, imagePath);
				future.complete();
			} else {
				log.warn("Shell script execution failed on " + imagePath + " , exit value: " + exitValue + ".");
				future.fail("Shell script execution failed on " + imagePath + ".");
			}
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
