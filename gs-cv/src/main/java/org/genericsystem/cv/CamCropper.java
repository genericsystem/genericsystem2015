package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.Classifier.CompareFeatureResult;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class CamCropper extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	// private final static String refPath = "classes/id-fr-front/image4-0.png";
	private final static String refPath = "classes/id-fr-front/template/template.png";

	private static Img ref = new Img(Imgcodecs.imread(refPath)).bgr2Gray();

	private final VideoCapture camera = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	private static boolean alreadyMatched = false;

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		camera.read(frame);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 0);
		mainGrid.add(ref.getImageView(), 0, 1);
		ImageView result = ref.getImageView();
		mainGrid.add(result, 0, 1);
		Mat[] average = new Mat[] { ref.getSrc().clone() };
		int n = 3;
		timer.scheduleAtFixedRate(() -> {
			camera.read(frame);
			Mat gray = new Mat();

			Imgproc.cvtColor(frame, gray, Imgproc.COLOR_BGR2GRAY);
			src.setImage(Tools.mat2jfxImage(frame));
			CompareFeatureResult adjusted = Classifier.compareFeature(frame, ref.getSrc(), 15, FeatureDetector.ORB, DescriptorExtractor.OPPONENT_ORB);
			if (adjusted != null)
				Core.addWeighted(average[0], (Double.valueOf(n) - 1d) / n, adjusted.getImg(), 1d / n, 0, average[0]);
			result.setImage(Tools.mat2jfxImage(average[0]));

		}, 0L, 33L, TimeUnit.MILLISECONDS);
	}

}
