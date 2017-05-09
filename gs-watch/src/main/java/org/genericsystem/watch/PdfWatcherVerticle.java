package org.genericsystem.watch;

import java.io.IOException;
import java.nio.file.ClosedWatchServiceException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class PdfWatcherVerticle extends AbstractVerticle {
	public static final String ADDRESS = "app.pdfchanges";
	private final Path folder;
	private WatchService watcher;

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PdfWatcherVerticle(Paths.get("..", "gs-cv", "pdf")));
	}

	public PdfWatcherVerticle(Path folder) {
		this.folder = folder;
	}

	@Override
	public void start() throws Exception {
		try {
			watcher = folder.getFileSystem().newWatchService();
			folder.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_DELETE);

			vertx.setPeriodic(1000, new Handler<Long>() {
				@Override
				public void handle(Long timerId) {
					processEvents();
				}
			});
		} catch (ClosedWatchServiceException | IOException e) {
			throw new IllegalStateException(e);
		}
	}

	public void processEvents() {
		while (true) {
			WatchKey key = watcher.poll();
			if (key == null)
				break;

			for (WatchEvent<?> event : key.pollEvents()) {
				@SuppressWarnings("unchecked")
				WatchEvent<Path> ev = (WatchEvent<Path>) event;
				WatchEvent.Kind<?> kind = ev.kind();

				if (kind == StandardWatchEventKinds.OVERFLOW)
					continue;

				Path filename = ev.context();
				JsonObject watchMsg = new JsonObject();
				watchMsg.put("filename", filename.toString());
				watchMsg.put("folder", folder.toString());
				watchMsg.put("kind", kind.name());

				vertx.eventBus().publish(ADDRESS, watchMsg.encodePrettily());
			}

			boolean valid = key.reset();
			if (!valid) {
				break;
			}
		}
	}

	@Override
	public void stop() {
		try {
			watcher.close();
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
