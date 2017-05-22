package org.genericsystem.watch;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;

public class PdfsConverterVerticle extends AbstractVerticle {

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PdfsConverterVerticle());
	}

	@Override
	public void start() {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.PDF_WATCHER_ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			Path newFile = Paths.get(message.body());
			System.out.println(">> New PDF file: " + newFile);
			List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File("../gs-cv/png"));
			for (Path path : createdPngs)
				vertx.eventBus().publish(VerticleDeployer.PNG_WATCHER_ADDRESS, path.toString());
			System.gc();
			System.runFinalization();
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
