package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.text.ERFilter;
import org.opencv.text.OCRTesseract;
import org.opencv.text.Text;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class TextDetector extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	// private final static String imgClassDirectory = "classes/id-fr-front";
	private final VideoCapture camera = new VideoCapture(0);
	private final ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private final OCRTesseract ocr = OCRTesseract.create("/usr/share/tesseract-ocr/4.00/", "fra", "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<'", 1, 7);

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		ImageView imgView = new ImageView();
		mainGrid.add(imgView, 0, 0);
		Mat frame = new Mat();
		camera.read(frame);
		imgView.setImage(Tools.mat2jfxImage(frame));
		timer.scheduleAtFixedRate(() -> {
			camera.read(frame);
			detect(frame);
			imgView.setImage(Tools.mat2jfxImage(frame));

		}, 0L, 33L, TimeUnit.MILLISECONDS);
	}

	public void detect(Mat src) {
		ERFilter er_filter1 = Text.createERFilterNM1("resources/trained_classifierNM1.xml", 16, 0.00015f, 0.13f, 0.2f, true, 0.1f);
		ERFilter er_filter2 = Text.createERFilterNM2("resources/trained_classifierNM2.xml", 0.5f);
		MatOfRect groups_rects = new MatOfRect();
		Text.detectRegions(src, er_filter1, er_filter2, groups_rects, Text.ERGROUPING_ORIENTATION_HORIZ, "resources/trained_classifier_erGrouping.xml", 0.5f);
		for (Rect rect : groups_rects.toArray()) {
			if (rect.tl().x >= 0 && rect.tl().y >= 0 && rect.br().x < src.cols() && rect.br().y < src.height()) {
				Mat bordered = new Mat(src, rect);
				//Core.copyMakeBorder(bordered, bordered, 15, 15, 15, 15, Core.BORDER_CONSTANT, new Scalar(0));
				System.out.println(ocr.run(bordered, 50, 1));
				// System.out.println(Ocr.doWork(bordered));
				Imgproc.rectangle(src, rect.tl(), rect.br(), src.type() == CvType.CV_8UC3 ? new Scalar(0, 255, 0) : new Scalar(255), 1, Imgproc.LINE_8, 0);
			}
		}

	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		camera.release();
		super.stop();
	}
}