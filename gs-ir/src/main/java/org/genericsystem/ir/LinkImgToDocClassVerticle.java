package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.newmodel.FillNewModelWithData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrPersistenceVerticle persists the informations from the OCR into Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class LinkImgToDocClassVerticle extends ActionPersistentVerticle {

	public static final String ACTION = "linkImgToDocClass";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public LinkImgToDocClassVerticle(Root engine) {
		super(engine);
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path filePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		String defaultClass = "unclassified";
		logger.info("Classifying {} as {}", filePath.getFileName(), defaultClass);
		try {
			FillNewModelWithData.linkImgToDocClass(engine, filePath, defaultClass);
			future.complete();
		} catch (RuntimeException e) {
			future.fail(e);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.failed())
			throw new IllegalStateException("Exception in OcrPersistenceVerticle.", res.cause());
	}

}
