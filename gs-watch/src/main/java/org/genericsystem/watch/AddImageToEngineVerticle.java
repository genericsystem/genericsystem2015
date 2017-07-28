package org.genericsystem.watch;

import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;

import com.sun.xml.internal.ws.api.pipe.Engine;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The AddImageToEngineVerticle receives a message from the event bus when an image was classified. After saving the document in Generic System, a message is sent to the {@link DezonerVerticle}. A reference to an {@link Engine} must be given to be able to
 * store the data in Generic System.
 * 
 * @author Pierrik Lassalas
 */
public class AddImageToEngineVerticle extends ActionVerticle {

	public static final String ACTION = "newImage";

	private Root engine;

	/**
	 * Default constructor. A reference to the engine must be provided.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public AddImageToEngineVerticle(Root engine) {
		this.engine = engine;
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = task.getString(DistributedVerticle.FILENAME);
		boolean result;
		if (null != engine)
			result = FillModelWithData.registerNewFile(engine, Paths.get(imagePath));
		else
			result = FillModelWithData.registerNewFile(Paths.get(imagePath));
		if (result)
			future.complete(imagePath);
		else
			future.fail("An error has occured while saving file " + imagePath);
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask((String) res.result(), DezonerVerticle.ACTION);
		else
			throw new IllegalStateException("An error has occurred while saving file " + task.getString(DistributedVerticle.FILENAME));
	}
}
