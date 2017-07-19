package org.genericsystem.watch.gamma;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import org.genericsystem.cv.PdfToPngConverter;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PdfConverterVerticle extends ActionVerticle {

	public PdfConverterVerticle(String privateAddress, String privatePath, String ip, List<JsonObject> messages, List<JsonObject> tasks) {
		super(privateAddress, privatePath, ip, messages, tasks);
	}

	public static final String ACTION = "pdfToPng";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, String fileName, JsonObject task) {
		File file = new File(getPrivatePath() + fileName);
		List<Path> createdPngs = PdfToPngConverter.convertPdfToImages(file, new File("../gs-cv/png"));
		future.complete(createdPngs);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, String fileName) {
		if (res.succeeded()) {
			for (Path newPng : (List<Path>) res.result()) {
				long id = System.currentTimeMillis();
				getMessages().add(new JsonObject().put(DistributedVerticle.ID, id).put("task", new JsonObject().put(DistributedVerticle.ID, id)
						.put(DistributedVerticle.FILENAME, newPng.toString()).put(DistributedVerticle.IP, getIp()).put(DistributedVerticle.TYPE, ClassifierVerticle.ACTION)));
				System.out.println("New PNG file : " + newPng);
			}
		}
	}
}
