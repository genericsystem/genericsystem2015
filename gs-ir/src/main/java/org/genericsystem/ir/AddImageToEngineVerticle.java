package org.genericsystem.ir;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The AddImageToEngineVerticle receives a message from the event bus when an image was classified. After saving the document in Generic System, a message is sent to the {@link DezonerVerticle}. A reference to an {@link Engine} must be given to be able to
 * store the data in Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class AddImageToEngineVerticle extends ActionPersistentVerticle {

	public static final String ACTION = "newImage";

	public AddImageToEngineVerticle(Root engine) {
		super(engine);
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME);
		System.out.println("--- imagePath = " + imagePath);
		boolean result = FillModelWithData.registerNewFile(engine, Paths.get(imagePath), DistributedVerticle.RESOURCES_FOLDER);
		if (result)
			future.complete();
		else
			future.fail("An error has occured while saving file " + imagePath);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), DezonerVerticle.ACTION);
		else
			throw new IllegalStateException("An error has occurred while saving file " + task.getString(DistributedVerticle.FILENAME), res.cause());
	}
}
