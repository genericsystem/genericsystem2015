package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

public class PngWatcherVerticle extends DirectoryWatcherVerticle {
	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PngWatcherVerticle(Paths.get("..", "gs-cv", "png"), VerticleDeployer.PNG_WATCHER_ADDRESS));
	}

	public PngWatcherVerticle(Path folder, String address) {
		super(folder, address);
	}
}
