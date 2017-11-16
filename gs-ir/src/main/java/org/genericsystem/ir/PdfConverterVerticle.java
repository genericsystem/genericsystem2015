package org.genericsystem.ir;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The PdfConverterVerticle receives a message from the event bus when a new PDF was added. The file is processed, and each page is converted to a PNG. Finally, a message is sent to the {@link ClassifierVerticle}.
 */
public class PdfConverterVerticle extends ActionVerticle {

	public static final String ACTION = "pdfToPng";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		File file = new File(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(file, new File(DistributedVerticle.BASE_PATH + "converted-png"));
		future.complete(createdPngs);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			for (Path newPng : (List<Path>) res.result()) {
				addTask(Paths.get(DistributedVerticle.BASE_PATH).relativize(newPng).toString(), DeskewerVerticle.ACTION);
				// addTask(Paths.get(DistributedVerticle.BASE_PATH).relativize(newPng).toString(), ClassifierVerticle.ACTION);
			}
		} else {
			throw new IllegalStateException("An error has occured while extracting images from PDF", res.cause());
		}
	}
}
