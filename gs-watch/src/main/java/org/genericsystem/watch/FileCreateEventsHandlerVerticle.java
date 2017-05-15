package org.genericsystem.watch;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardWatchEventKinds;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;

public abstract class FileCreateEventsHandlerVerticle extends AbstractVerticle {
	private final String address;

	public FileCreateEventsHandlerVerticle(String address) {
		this.address = address;
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(address);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			System.out.println();
			System.out.println("Message, address: " + message.address());
			System.out.println("       , body: " + message.body());
			JsonObject json = new JsonObject(message.body());
			String kind = json.getString("kind");
			if (StandardWatchEventKinds.ENTRY_CREATE.name().equals(kind)) {
				Path watchedDir = Paths.get(".", json.getString("folder").split("/"));
				Path newFile = watchedDir.resolve(json.getString("filename"));
				try (FileLock lock = new RandomAccessFile(newFile.toFile(), "rw").getChannel().lock()) {
					handle(newFile);
				} catch (IOException e) {
					throw new IllegalStateException(e);
				}
			}
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}

	public abstract void handle(Path newFile);
}
