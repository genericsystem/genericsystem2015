package org.genericsystem.watch;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.ZoneScorer;
import org.genericsystem.cv.Zones;
import org.opencv.core.Size;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OcrVerticle extends AbstractVerticle {

	private static Logger log = LoggerFactory.getLogger(ClassifierVerticle.class);

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new OcrVerticle());
	}

	@Override
	public void start() throws Exception {
		MessageConsumer<String> consumer = vertx.eventBus().consumer(VerticleDeployer.IMAGE_TO_OCR);
		consumer.handler(message -> vertx.executeBlocking(future -> {
			String imagePath = message.body();
			System.out.println(">>>>> New image to OCR: " + imagePath);
			Path imgClassDirectory = Paths.get(imagePath).getParent();
			ImgClass imgClass = ImgClass.fromDirectory(imgClassDirectory.toString());
			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(15, 3)));
			Zones zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
			for (Zone zone : zones.get()) {
				ZoneScorer scorer = zone.newUnsupervisedScorer(Tools.classImgsStream(imgClassDirectory + "/mask/" + Paths.get(imagePath).getFileName().toString().replace(".png", "")));
				System.out.println("Image " + imagePath + ", found text: " + scorer.getBestText() + " " + Math.floor((scorer.getBestScore() * 10000)) / 100 + "%");
			}
			System.gc();
			System.runFinalization();
			future.complete();
		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		}));
	}
}
