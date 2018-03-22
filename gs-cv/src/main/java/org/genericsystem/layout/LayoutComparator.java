package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.DirectionalFilter;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

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
		mainGrid.add(src2, 2, 0);
		ImageView src3 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src3, 3, 0);
		ImageView src4 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src4, 4, 0);
		ImageView src5 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src5, 0, 1);
		ImageView src6 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src6, 1, 1);
		ImageView src7 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src7, 2, 1);
		ImageView src8 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src8, 3, 1);
		ImageView src9 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src9, 4, 1);

		DirectionalFilter df = new DirectionalFilter();

		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img frameImg = new Img(frame, false);
				Img gray = frameImg.bgr2Gray();

				Mat gx = df.gx(gray.getSrc());
				Mat gy = df.gy(gray.getSrc());
				Core.subtract(Mat.zeros(gy.size(), gy.type()), gy, gy);
				Mat mag = new Mat();
				Mat ori = new Mat();
				// Core.subtract(Mat.zeros(gy.size(), gy.type()), gy, gy);
				// Core.subtract(Mat.zeros(gx.size(), gx.type()), gx, gx);
				Core.cartToPolar(gx, gy, mag, ori);

				int[][] binning = df.bin(ori, 64);

				Mat mask = new Mat();

				Mat result = Mat.zeros(mag.size(), CvType.CV_64FC1);
				mag.copyTo(result, mask);
				System.out.println((int) Core.sumElems(result).val[0]);
				Core.multiply(result, new Scalar(10), result, 1);

				Img img0 = new Img(result, false);
				// Img img0 = frameImg.bilateralFilter(10, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11, 3));

				Img img1 = frameImg.canny(60, 180).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11, 3));
				Img img2 = frameImg.bgr2Gray().grad(2.0d, 2.0d).thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU).bitwise_not().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(11, 3));
				Img img3 = frameImg.sauvolaThreshold().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11, 3));
				Img img4 = frameImg.bgr2Gray().gaussianBlur(new Size(3, 3)).absDiff(new Scalar(255.0)).adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, 11, 3).bitwise_not().morphologyEx(Imgproc.MORPH_CLOSE,
						Imgproc.MORPH_RECT, new Size(11, 3));

				// Layout layout = img0.buildLayout();
				// Layout layout1 = img1.buildLayout();
				// Layout layout2 = img2.buildLayout();
				// Layout layout3 = img3.buildLayout();
				// Layout layout4 = img4.buildLayout();
				//
				// Img out = new Img(frame, true);
				// layout.draw(img0, new Scalar(0), 1);
				// Img out1 = new Img(frame, true);
				// layout1.draw(img1, new Scalar(0), 1);
				// Img out2 = new Img(frame, true);
				// layout2.draw(img2, new Scalar(0), 1);
				// Img out3 = new Img(frame, true);
				// layout3.draw(img3, new Scalar(0), 1);
				// Img out4 = new Img(frame, true);
				// layout4.draw(img4, new Scalar(0), 1);

				src.setImage(img0.toJfxImage());
				src1.setImage(img1.toJfxImage());
				src2.setImage(img2.toJfxImage());
				src3.setImage(img3.toJfxImage());
				src4.setImage(img4.toJfxImage());
				// src5.setImage(detectContours(frameImg, img0).toJfxImage());
				src6.setImage(detectContours(frameImg, img1).toJfxImage());
				src7.setImage(detectContours(frameImg, img2).toJfxImage());
				src8.setImage(detectContours(frameImg, img3).toJfxImage());
				src9.setImage(detectContours(frameImg, img4).toJfxImage());

			} catch (Throwable t) {
				t.printStackTrace();
			}
		}, 400, 10, TimeUnit.MILLISECONDS);
	}

	Img detectContours(Img frame, Img binary) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(binary.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 10;
		Img result = new Img(frame.getSrc(), true);
		contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).peek(c -> Imgproc.drawContours(result.getSrc(), Arrays.asList(c), 0, new Scalar(0, 255, 0))).peek(c -> {
			Point[] pts = new Point[4];
			Imgproc.minAreaRect(new MatOfPoint2f(c.toArray())).points(pts);
			Imgproc.drawContours(result.getSrc(), Arrays.asList(new MatOfPoint(pts)), 0, new Scalar(255, 0, 0), 2);
		}).count();
		/* .map(Imgproc::boundingRect).forEach(rect -> Imgproc.rectangle(result.getSrc(), rect.br(), rect.tl(), new Scalar(0, 0, 255), 1)) */;
		return result;
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}

}