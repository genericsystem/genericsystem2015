package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Mat;
import org.opencv.core.MatOfFloat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.text.TextDetectorCNN;
import org.opencv.utils.Converters;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class CNNTextDetector extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	private Img frame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };
	private final TextDetectorCNN cnn = TextDetectorCNN.create("resources/textbox.prototxt", "resources/TextBoxes_icdar13.caffemodel");

	private void startTimer() {
		timer.scheduleAtFixedRate(() -> {
			try {
				Image[] images = doWork();
				if (images != null)
					Platform.runLater(() -> {
						Iterator<Image> it = Arrays.asList(images).iterator();
						for (int row = 0; row < imageViews.length; row++)
							for (int col = 0; col < imageViews[row].length; col++)
								if (it.hasNext())
									imageViews[row][col].setImage(it.next());
					});
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1;
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(frame.width() / displaySizeReduction);
				imageView.setFitHeight(frame.height() / displaySizeReduction);
			}
		startTimer();
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			frame = gsCapture.read();

		Image[] images = new Image[3];
		long ref = System.currentTimeMillis();
		long ref2 = System.currentTimeMillis();
		MatOfRect mor = new MatOfRect();
		MatOfFloat mof = new MatOfFloat();
		cnn.detect(frame.getSrc(), mor, mof);

		List<Rect> rects = new ArrayList<>();
		Converters.Mat_to_vector_Rect(mor, rects);

		List<Float> confidences = new ArrayList<>();
		Converters.Mat_to_vector_float(mof, confidences);
		Mat display = frame.getSrc().clone();
		for (int rectIndex = 0; rectIndex < rects.size(); rectIndex++) {
			if (confidences.get(rectIndex) >= 0.1)
				Imgproc.rectangle(display, rects.get(rectIndex), new Scalar(0, 255, 0));
			System.out.println("confidence : " + confidences.get(rectIndex) + " " + rects.get(rectIndex));
		}
		images[0] = new Img(display, false).toJfxImage();
		ref = trace("cnn detection", ref);
		ref2 = trace("total", ref2);

		return images;
	}

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn)
			timer.shutdown();
		else {
			timer = new BoundedScheduledThreadPoolExecutor();
			startTimer();
		}
		config.isOn = !config.isOn;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}

}
