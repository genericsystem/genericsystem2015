package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDouble;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class LinesDetector extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		capture.read(frame);

		ImageView canny = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(canny, 0, 0);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 1);
		ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src2, 1, 0);
		double sigma = 0.33;
		timer.scheduleAtFixedRate(() -> {
			capture.read(frame);

			Img grad = new Img(frame).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();

			Img gray = new Img(frame).bgr2Gray();
			// Img blured = gray.gaussianBlur(new Size(3, 3));

				MatOfDouble mu = new MatOfDouble();
				Core.meanStdDev(gray.getSrc(), mu, new MatOfDouble());
				double median = mu.get(0, 0)[0];
				double lower = Math.max(0, (1.0 - sigma) * median);
				double upper = Math.min(255, (1.0 + sigma) * median);
				Img edges = gray.canny(lower, upper);
				Imgproc.dilate(edges.getSrc(), edges.getSrc(), new Mat());

				Img lines = edges.houghLinesP(1, Math.PI / 180, 50, 100, 10);
				Img frameCopy = new Img(frame);

				double angle = 0.;
				for (int i = 0; i < lines.rows(); i++) {
					double[] val = lines.get(i, 0);
					// Imgproc.line(frame, new Point(val[0], val[1]), new Point(val[2], val[3]), new Scalar(0, 0, 255), 1);
					angle += Math.atan2(val[3] - val[1], val[2] - val[0]);
				}
				angle /= lines.rows(); // mean angle, in radians.

				double j = 0;
				double goodAngle = 0;
				double angle_tmp;
				for (int i = 0; i < lines.rows(); i++) {
					double[] val = lines.get(i, 0);
					angle_tmp = Math.atan2(val[3] - val[1], val[2] - val[0]);
					if (Math.abs((angle - angle_tmp) / Math.PI * 180) < 10) {
						Imgproc.line(frame, new Point(val[0], val[1]), new Point(val[2], val[3]), new Scalar(0, 0, 255), 1);
						goodAngle += angle_tmp;
						j++;
					}

				}
				goodAngle /= j; // mean angle, in radians.

				Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), goodAngle / Math.PI * 180, 1);
				Mat rotated = new Mat();
				Imgproc.warpAffine(frameCopy.getSrc(), rotated, matrix, new Size(frame.size().width, frame.size().height));
				double crop = 0.20;
				Img croppedImg = new Img(new Mat(rotated, new Rect(Double.valueOf(rotated.width() * crop).intValue(), Double.valueOf(rotated.height() * crop).intValue(), Double.valueOf(rotated.width() * (1 - 2 * crop)).intValue(), Double.valueOf(
						rotated.height() * (1 - 2 * crop)).intValue())));

				canny.setImage(Tools.mat2jfxImage(edges.getSrc()));
				src.setImage(Tools.mat2jfxImage(frame));
				src2.setImage(Tools.mat2jfxImage(croppedImg.getSrc()));
				System.out.println("Average angle: " + Math.round(angle / Math.PI * 180));
			}, 0, 33, TimeUnit.MILLISECONDS);

	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}

}
