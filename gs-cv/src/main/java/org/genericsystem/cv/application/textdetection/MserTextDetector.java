package org.genericsystem.cv.application.textdetection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.BoundedScheduledThreadPoolExecutor;
import org.genericsystem.cv.application.Config;
import org.genericsystem.cv.application.GSCapture;
import org.genericsystem.cv.application.GSVideoCapture;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.MSER;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class MserTextDetector extends AbstractApp {

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
	private final MSER detector = MSER.create(1, 6, 40, 0.25, 0.2, 200, 1.01, 0.03, 5);

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

		// Mat sharpen = sharpen(frame.getSrc(), 1, 5, 1);

		ArrayList<MatOfPoint> regions = new ArrayList<>();
		MatOfRect mor = new MatOfRect();
		detector.detectRegions(frame.bgr2Gray().getSrc(), regions, mor);
		List<Rect> rects = new ArrayList<>();
		Converters.Mat_to_vector_Rect(mor, rects);
		Mat display = frame.getSrc().clone();

		rects.removeIf(rect -> {
			for (Rect other : rects)
				if (!other.equals(rect)) {
					if (contains(other, rect))
						return true;
					if (isOverlapping(rect, other))
						if (rect.area() < other.area())
							return true;
				}
			return false;
		});

		for (Rect rect : rects) {
			boolean isOverlapping = false;
			for (Rect other : rects)
				if (!(other.equals(rect))) {
					if (isOverlapping(rect, other)) {
						if (rect.area() < other.area())
							isOverlapping = true;
						break;
					}
				}
			Imgproc.rectangle(display, rect, isOverlapping ? new Scalar(0, 0, 255) : new Scalar(0, 255, 0), 1);
		}
		images[0] = new Img(display, false).toJfxImage();
		ref = trace("mser", ref);

		ref2 = trace("total", ref2);

		return images;
	}

	public boolean isEnoughOverlapping(Rect rect, Rect shiftedRect, int pts) {
		return (Math.abs(rect.tl().x - shiftedRect.tl().x) < pts) && (Math.abs(rect.tl().y - shiftedRect.tl().y) < pts) && (Math.abs(rect.br().x - shiftedRect.br().x) < pts) && (Math.abs(rect.br().y - shiftedRect.br().y) < pts);
	}

	private boolean contains(Rect rect, Rect shiftedRect) {
		return (rect.tl().x <= shiftedRect.tl().x && rect.tl().y <= shiftedRect.tl().y && rect.br().x >= shiftedRect.br().x && rect.br().y >= shiftedRect.br().y);
	}

	private boolean isOverlapping(Rect first, Rect other) {
		return first.tl().x < other.br().x && other.tl().x < first.br().x && first.tl().y < other.br().y && other.tl().y < first.br().y;
	}

	private Mat sharpen(Mat img, double sigma, double threshold, double amount) {
		Mat blurred = new Mat();
		Imgproc.GaussianBlur(frame.getSrc(), blurred, new Size(0, 0), sigma, sigma);
		Mat lowContrastMask = new Mat();
		Core.absdiff(frame.getSrc(), blurred, lowContrastMask);
		Imgproc.threshold(lowContrastMask, lowContrastMask, threshold, 255, Imgproc.THRESH_BINARY_INV);
		Mat sharpened = new Mat();
		Core.addWeighted(frame.getSrc(), 1 + amount, blurred, -amount, 0, sharpened);
		frame.getSrc().copyTo(sharpened, lowContrastMask);
		return sharpened;
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
