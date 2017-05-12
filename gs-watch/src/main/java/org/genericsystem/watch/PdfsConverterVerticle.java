package org.genericsystem.watch;

import java.io.File;
import java.nio.file.Path;

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
		PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File("../gs-cv/png"));
	}
}
