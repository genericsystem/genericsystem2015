package org.genericsystem.watch;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardWatchEventKinds;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;

public class PdfsConverterVerticle extends AbstractVerticle {

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new PdfsConverterVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(PdfWatcherVerticle.ADDRESS);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			JsonObject json = new JsonObject(message.body());
			String kind = json.getString("kind");
			if (StandardWatchEventKinds.ENTRY_CREATE.name().equals(kind) || StandardWatchEventKinds.ENTRY_MODIFY.name().equals(kind)) {
				Path watchedDir = Paths.get(".", json.getString("folder").split("/"));
				Path newFile = watchedDir.resolve(json.getString("filename"));
				FileChannel channel = null;
				try {
					RandomAccessFile raf = new RandomAccessFile(newFile.toFile(), "rw");
					channel = raf.getChannel();
					channel.lock();
					PdfToPngConverter.convertPdfToImages(newFile.toFile(), new File("../gs-cv/png"));
					channel.close();
					raf.close();
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
}
