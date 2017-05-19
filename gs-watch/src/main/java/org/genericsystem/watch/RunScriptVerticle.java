package org.genericsystem.watch;

import java.io.IOException;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class RunScriptVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);
	private static final String shellScript = "../gs-cv/didjvu.sh";

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new RunScriptVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_ADDED_TO_CLASS_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			System.out.println(">>>> New classified image: " + message.body());
			try {
				Process process = Runtime.getRuntime().exec(new String[] { shellScript, message.body() });
				int exitValue = process.waitFor();
				if (exitValue != 0)
					log.warn("Shell script execution failed, exit value: " + exitValue);
			} catch (IOException | InterruptedException e) {
				log.warn("Exception while executing shell script on file " + message.body(), e);
			}
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
