package org.genericsystem.watch;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

public class PdfsConverterVerticle extends FileCreateEventsHandlerVerticle {

	public PdfsConverterVerticle() {
		super(VerticleDeployer.PDF_WATCHER_ADDRESS);
	}

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PdfsConverterVerticle());
	}

	@Override
	public void handle(Path newFile) {
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File("../gs-cv/png"));
		for (Path path : createdPngs)
			vertx.eventBus().publish(VerticleDeployer.PNG_WATCHER_ADDRESS, path.toString());
	}
}
