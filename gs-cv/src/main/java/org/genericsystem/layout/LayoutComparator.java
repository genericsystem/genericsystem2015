package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class LayoutComparator extends AbstractApp {

	static {
		NativeLibraryLoader.load();
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
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 0);
		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src1, 1, 0);
		ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src2, 0, 1);
		ImageView src3 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src3, 1, 1);
		ImageView src4 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src4, 2, 1);
		Mat average = adjust(frame);
		double n = 20;
		timer.scheduleAtFixedRate(() -> {
			capture.read(frame);
			Img frameImg = new Img(frame, false);
			try {

				Mat currentAdjustedFrame = adjust(frame);
				Core.addWeighted(average, (n - 1) / n, currentAdjustedFrame, 10d / n, 0, average);
				Mat diff = computeDiffFrame(currentAdjustedFrame, average);

				Img img0 = frameImg.bilateralFilter(30, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, 17, 15);
				Img img1 = frameImg.canny(60, 200).bitwise_not();
				Img img2 = frameImg.bgr2Gray().grad(2.5d, 2.5d).thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU);
				Img img3 = frameImg.sauvolaThreshold().bitwise_not();

				List<MatOfPoint> contours = new ArrayList<>();
				Imgproc.findContours(diff, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
				double minArea = 100;
				Img img4 = new Img(new Mat(frame.size(), CvType.CV_8U, new Scalar(255)), false);
				contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).forEach(c -> Imgproc.drawContours(img4.getSrc(), Arrays.asList(c), 0, new Scalar(0)));// map(Imgproc::boundingRect).forEach(rect ->);

				Layout layout = img0.buildLayout();
				Layout layout1 = img1.buildLayout();
				Layout layout2 = img2.buildLayout();
				Layout layout3 = img3.buildLayout();
				Layout layout4 = img4.buildLayout();

				Img out = new Img(frame, true);
				layout.draw(img0, new Scalar(0), 1);
				Img out1 = new Img(frame, true);
				layout1.draw(img1, new Scalar(0), 1);
				Img out2 = new Img(frame, true);
				layout2.draw(img2, new Scalar(0), 1);
				Img out3 = new Img(frame, true);
				layout3.draw(img3, new Scalar(0), 1);
				Img out4 = new Img(frame, true);
				layout4.draw(img4, new Scalar(0), 1);

				src.setImage(img0.toJfxImage());
				src1.setImage(img1.toJfxImage());
				src2.setImage(img2.toJfxImage());
				src3.setImage(img3.toJfxImage());
				src4.setImage(Tools.mat2jfxImage(diff));
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}, 400, 10, TimeUnit.MILLISECONDS);
	}

	public static Mat computeDiffFrame(Mat currentAdjustedFrame, Mat prevAdjustedFrame) {
		Mat result = new Mat();
		Core.absdiff(currentAdjustedFrame, prevAdjustedFrame, result);
		Imgproc.adaptiveThreshold(result, result, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 9, 3);
		return result;
	}

	public static Mat adjust(Mat frame) {
		Mat result = frame.clone();
		Imgproc.cvtColor(frame, result, Imgproc.COLOR_BGR2GRAY);
		Imgproc.GaussianBlur(result, result, new Size(3, 3), 0);
		// Imgproc.Canny(result, result, 150d, 150d * 2, 3, true);
		return result;
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}

}