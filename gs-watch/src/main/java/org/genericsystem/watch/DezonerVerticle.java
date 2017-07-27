package org.genericsystem.watch;

import org.genericsystem.cv.Zones;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

/**
 * The DezonerVerticle receives a message from the event bus when a new image has been added to a class. If this class has already been dezoned, a message will be sent to the {@link OcrVerticle}.
 * 
 * @author Pierrik Lassalas
 */
public class DezonerVerticle extends AbstractVerticle {

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new DezonerVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.NEW_IMAGE_ADDED_TO_CLASS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			System.out.println(">>> dezoner: " + Thread.currentThread().getName());

			String imagePath = message.body();
			System.out.println(">>>>> New image to de-zone: " + imagePath);
			if (Zones.isZonesFilePresent(imagePath)) {
				// The zones file was found, proceed trough OCR directly
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_FOUND, imagePath);
				future.complete();
			} else {
				// No zones file was found, need to define the zones manually
				vertx.eventBus().publish(VerticleDeployer.ACCURATE_ZONES_NOT_FOUND, imagePath);
				// TODO: replace the future.fail by a notification to the system that a zone needs to be defined for this file
				future.fail("No accurate zones found for " + imagePath);
			}
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
