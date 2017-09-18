package org.genericsystem.ir;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The PdfConverterVerticle receives a message from the event bus when a new PDF was added. The file is processed, and each page is converted to a PNG. Finally, a message is sent to the {@link ClassifierVerticle}.
 * 
 * @author middleware
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

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			for (Path newPng : (List<Path>) res.result())
				addTask(newPng.toString().replaceFirst(DistributedVerticle.BASE_PATH, ""), ClassifierVerticle.ACTION);
		}
	}
}
