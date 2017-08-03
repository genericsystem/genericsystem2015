package org.genericsystem.watch;

import java.lang.invoke.MethodHandles;
import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.xml.internal.ws.api.pipe.Engine;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The OcrVerticle receives a message from the event bus when an image has been successfully de-zoned. The image is then processed (e.g., transformed and OCR'd) and the results from the OCR are stored in Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class OcrVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static final String ACTION = "ocr";
	public static final String NEW_IMAGE_PROCESSED = "app.ocr.newimage.processed";
	public static final String KNOWN_IMAGE_PROCESSED = "app.ocr.knownimage.updated";
	public static final String KNOWN_IMAGE_PASSED = "app.ocr.knownimage.passed";

	@Override
	public String getAction() {
		return ACTION;
	}

	private Root engine;

	/**
	 * Default constructor. A reference to an {@link Engine} must be provided to be able to save the results.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public OcrVerticle(Root engine) {
		this.engine = engine;
	}

	// TODO: refactor the code, since the file is now stored before being sent to OCR (and thus, is always known)
	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = task.getString(DistributedVerticle.FILENAME);

		int result = FillModelWithData.ERROR;
		if (null != engine)
			result = FillModelWithData.doImgOcr(engine, Paths.get(imagePath));
		else
			result = FillModelWithData.doImgOcr(Paths.get(imagePath));

		switch (result) {
			case FillModelWithData.NEW_FILE:
				logger.debug("New image (processed) {}.", imagePath);
				future.complete(NEW_IMAGE_PROCESSED);
				break;
			case FillModelWithData.KNOWN_FILE:
				logger.debug("Known image (passed) {} ", imagePath);
				future.complete(KNOWN_IMAGE_PASSED);
				break;
			case FillModelWithData.KNOWN_FILE_UPDATED_FILTERS:
				logger.debug("Known image (updated) {} ", imagePath);
				future.complete(KNOWN_IMAGE_PROCESSED);
				break;
			default:
				future.fail("Unhandled case!");
				break;
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), (String) res.result());
		else
			throw new IllegalStateException("Exception in OcrVerticle.", res.cause());
	}
}
