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
 * The DezonerVerticle receives a message from the event bus when a new image has been added to a class. If this class has already been dezoned, a message will be sent to the {@link OcrParametersVerticle}.
 * 
 * @author Pierrik Lassalas
 */
public class DezonerVerticle extends ActionPersistentVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String ACTION = "dezoner";

	public DezonerVerticle(Root engine) {
		super(engine);
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		Path imagePath = Paths.get(DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME));
		JsonObject fields = FillNewModelWithData.detectFields(imagePath);
		if (null == fields || fields.isEmpty())
			future.fail("No fields detected for image " + task.getString(DistributedVerticle.FILENAME));
		else
			future.complete(fields);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded()) {
			addTask(task.getString(DistributedVerticle.FILENAME), (JsonObject) res.result(), AnnotateImageVerticle.ACTION);
			addTask(task.getString(DistributedVerticle.FILENAME), (JsonObject) res.result(), OcrWorkerVerticle.ACTION);
		} else {
			logger.info(String.format("No zones defined for file {}.", task.getString(DistributedVerticle.FILENAME)), res.cause());
		}
	}
}
