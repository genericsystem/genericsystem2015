package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

public class PdfWatcherVerticle extends DirectoryWatcherVerticle {
	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PdfWatcherVerticle(Paths.get("..", "gs-cv", "pdf"), VerticleDeployer.PDF_WATCHER_ADDRESS));
	}

	public PdfWatcherVerticle(Path folder, String address) {
		super(folder, address);
	}
}
