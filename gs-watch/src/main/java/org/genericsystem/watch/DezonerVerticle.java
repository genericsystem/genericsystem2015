package org.genericsystem.watch;

import java.nio.file.Paths;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.ModelTools;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The DezonerVerticle receives a message from the event bus when a new image has been added to a class. If this class has already been dezoned, a message will be sent to the {@link OcrVerticle}.
 * 
 * @author Pierrik Lassalas
 */
public class DezonerVerticle extends ActionVerticle {

	public static final String ACTION = "dezoner";
	private static final String RESOURCES_FOLDER = System.getProperty("user.dir") + "/../gs-watch/src/main/resources/";

	@Override
	public String getAction() {
		return ACTION;
	}

	@Override
	protected void handle(Future<Object> future, JsonObject task) {
		String imagePath = task.getString(DistributedVerticle.FILENAME);
		if (Zones.isZonesFilePresent(imagePath)) {
			final Zones zones = Zones.load(Paths.get(imagePath).getParent().toString());
			Img imgCopy = new Img(imagePath);
			zones.draw(imgCopy, new Scalar(0, 255, 0), 3);
			zones.writeNum(imgCopy, new Scalar(0, 0, 255), 3);
			// TODO implement a filter mechanism to avoid creating duplicates in a public folder
			String filenameExt = ModelTools.generateFileName(Paths.get(imagePath));
			Imgcodecs.imwrite(RESOURCES_FOLDER + filenameExt, imgCopy.getSrc());
			imgCopy.close();
			future.complete(OcrVerticle.ACTION);
		} else {
			// No zones file was found, need to define the zones manually
			// TODO: replace the future.fail by a notification to the system that a zone needs to be defined for this file
			future.fail("No accurate zones found for " + imagePath);
		}
	}

	@Override
	protected void handleResult(AsyncResult<Object> res, JsonObject task) {
		if (res.succeeded())
			addTask(task.getString(DistributedVerticle.FILENAME), (String) res.result());
		else
			System.out.println("No zones defined for file " + task.getString(DistributedVerticle.FILENAME));
	}
}
