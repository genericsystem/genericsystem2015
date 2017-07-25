package org.genericsystem.watch.gamma;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PdfConverterVerticle extends ActionVerticle {

	public PdfConverterVerticle(String privateAddress, String privatePath, String ip) {
		super(privateAddress, privatePath, ip);
	}

	public static final String ACTION = "pdfToPng";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		File file = new File(task.getString(DistributedVerticle.FILENAME));
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(file, new File("../gs-cv/png"));
		future.complete(createdPngs);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			for (Path newPng : (List<Path>) res.result())
				addTask(newPng.toString(), getIp(), ClassifierVerticle.ACTION);
		}
	}
}
