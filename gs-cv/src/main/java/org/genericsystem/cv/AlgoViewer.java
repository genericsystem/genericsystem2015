package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class AlgoViewer extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final static String refPath = "classes/id-fr-front/image4-0.png";
	private static Img ref = new Img(refPath);
	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		capture.read(frame);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 0);
		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src1, 0, 1);
		ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src2, 1, 0);
		ImageView src3 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src3, 1, 1);
		ImageView src4 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src4, 2, 0);
		ImageView src5 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src5, 2, 1);

		timer.scheduleAtFixedRate(() -> {
			capture.read(frame);
			Img frameImg = new Img(frame, false);
			src.setImage(frameImg.toJfxImage());
			src1.setImage(frameImg.bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 5, -2).toJfxImage());
			src2.setImage(frameImg.canny(20, 80).toJfxImage());
			src3.setImage(frameImg.sobel().toJfxImage());
			src4.setImage(frameImg.otsu().toJfxImage());
			src5.setImage(frameImg.grad().toJfxImage());
		}, 0, 33, TimeUnit.MILLISECONDS);
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}

}