package org.genericsystem.ir;

import java.lang.invoke.MethodHandles;
import java.nio.file.Paths;

import org.genericsystem.common.Root;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
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
public class DezonerVerticle extends ActionVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String ACTION = "dezoner";

	private Root engine;

	/**
	 * Default constructor. A reference to an {@link Engine} must be provided to be able to save the results.
	 * 
	 * @param engine - the engine used to store the data
	 */
	public DezonerVerticle(Root engine) {
		this.engine = engine;
	}

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = DistributedVerticle.BASE_PATH + task.getString(DistributedVerticle.FILENAME);
		if (Zones.isZonesFilePresent(imagePath)) {
			final Zones zones = Zones.load(Paths.get(imagePath).getParent().toString());
			Img imgCopy = new Img(imagePath);
			zones.draw(imgCopy, new Scalar(0, 255, 0), 3);
			zones.writeNum(imgCopy, new Scalar(0, 0, 255), 3);
			// TODO implement a filter mechanism to avoid creating duplicates in a public folder
			String filenameExt = ModelTools.generateFileName(Paths.get(imagePath));
			Imgcodecs.imwrite(DistributedVerticle.RESOURCES_FOLDER + filenameExt, imgCopy.getSrc());
			imgCopy.close();
			future.complete();
		} else {
			// No zones file was found, need to define the zones manually
			// TODO: replace the future.fail by a notification to the system that a zone needs to be defined for this file
			future.fail("No accurate zones found for " + imagePath);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), OcrParametersVerticle.ACTION);
		else
			logger.info("No zones defined for file {}.", task.getString(DistributedVerticle.FILENAME));
	}
}
