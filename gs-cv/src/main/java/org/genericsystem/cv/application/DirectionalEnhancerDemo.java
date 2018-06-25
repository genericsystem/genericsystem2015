package org.genericsystem.cv.application;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Lines;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class DirectionalEnhancerDemo extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	private Img frame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };

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
		}, 1000, 30, TimeUnit.MILLISECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1.5;
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
		if (!config.stabilizedMode) {
			frame = gsCapture.read();
		}
		Image[] images = new Image[12];

		long ref = System.currentTimeMillis();

		// Mat mat = DirectionalEnhancer.prepare(superFrame.getFrame().getSrc());

		Mat mat = new Mat();
		Imgproc.cvtColor(frame.getSrc(), mat, Imgproc.COLOR_BGR2GRAY);
		Imgproc.GaussianBlur(mat, mat, new Size(13, 13), 0);
		images[0] = new Img(mat, false).toJfxImage();
		Imgproc.adaptiveThreshold(mat, mat, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 7, 2);
		images[1] = new Img(mat, false).toJfxImage();
		Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(9, 9)));
		// images[2] = new Img(mat, false).toJfxImage();
		// Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(5, 5)));

		images[2] = new Img(mat, false).toJfxImage();

		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(mat, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (MatOfPoint contour : contours)
			Imgproc.drawContours(mat, Arrays.asList(contour), 0, new Scalar(255, 0, 0), -1);
		images[3] = new Img(mat, false).toJfxImage();
		Mat gradient = new Mat();
		Imgproc.morphologyEx(mat, gradient, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(4, 4)));

		images[4] = new Img(gradient, false).toJfxImage();
		Mat smallLines = new Mat();
		Mat bigLines = new Mat();
		Imgproc.HoughLinesP(gradient, bigLines, 1, Math.PI / 180, 10, 30, 10);
		Imgproc.HoughLinesP(gradient, smallLines, 1, Math.PI / 180, 10, 30, 10);
		Lines slines = new Lines(smallLines);
		Lines blines = new Lines(bigLines);

		Mat result = Mat.zeros(frame.getSrc().size(), frame.getSrc().type());
		Lines horizontalLines = new Lines(blines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) < Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		horizontalLines.draw(result, new Scalar(0, 255, 0), 2);

		Lines verticalLines = new Lines(slines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) > Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		verticalLines.draw(result, new Scalar(0, 0, 255), 2);

		images[5] = new Img(result, false).toJfxImage();

		horizontalLines.draw(mat, new Scalar(255), 2);
		verticalLines.draw(mat, new Scalar(255), 2);
		ref = trace("Draw lines", ref);

		Imgproc.findContours(mat, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (MatOfPoint contour : contours)
			Imgproc.drawContours(mat, Arrays.asList(contour), 0, new Scalar(255, 0, 0), -1);

		images[6] = new Img(mat, false).toJfxImage();

		Imgproc.morphologyEx(mat, gradient, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));

		images[7] = new Img(gradient, false).toJfxImage();

		Imgproc.HoughLinesP(gradient, bigLines, 1, Math.PI / 180, 100, 300, 50);
		Imgproc.HoughLinesP(gradient, smallLines, 1, Math.PI / 180, 25, 40, 13);
		slines = new Lines(smallLines);
		blines = new Lines(bigLines);

		result = Mat.zeros(frame.getSrc().size(), frame.getSrc().type());
		horizontalLines = new Lines(blines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) < Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		horizontalLines.draw(result, new Scalar(0, 255, 0), 2);

		verticalLines = new Lines(slines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) > Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		verticalLines.draw(result, new Scalar(0, 0, 255), 2);

		images[8] = new Img(result, false).toJfxImage();

		mat.release();
		result.release();
		return images;
	}

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn) {
			timer.shutdown();
			// gsCapture.release();
		} else {
			timer = new BoundedScheduledThreadPoolExecutor();
			// gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
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
