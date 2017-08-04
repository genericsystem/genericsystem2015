package org.genericsystem.cv;

import java.util.Arrays;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.objdetect.CascadeClassifier;
import org.opencv.objdetect.Objdetect;
import org.opencv.videoio.VideoCapture;

public class FaceDetector extends AbstractApp {

	private static CascadeClassifier faceCascade;

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		faceCascade = new CascadeClassifier();
		faceCascade.load("resources/haarcascade_frontalface_alt2.xml");
	}

	// private final static String imgClassDirectory = "classes/id-fr-front";
	private VideoCapture camera = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

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
			Rect[] faces = detect(frame);
			Arrays.stream(faces).forEach(face -> Imgproc.rectangle(frame, face.tl(), face.br(), new Scalar(0, 255, 0), 1));
			imgView.setImage(Tools.mat2jfxImage(frame));

		}, 0L, 33L, TimeUnit.MILLISECONDS);
	}

	public static Rect[] detect(Mat frame) {

		Mat grayFrame = new Mat();
		// convert the frame in gray scale
		Imgproc.cvtColor(frame, grayFrame, Imgproc.COLOR_BGR2GRAY);
		// equalize the frame histogram to improve the result
		Imgproc.equalizeHist(grayFrame, grayFrame);
		MatOfRect faces = new MatOfRect();
		// compute minimum face size (20% of the frame height, in our case)
		int absoluteFaceSize = Math.round(grayFrame.rows() * 0.2f);
		if (absoluteFaceSize > 0) {
			// detect faces
			faceCascade.detectMultiScale(grayFrame, faces, 1.1, 2, 0 | Objdetect.CASCADE_SCALE_IMAGE, new Size(absoluteFaceSize, absoluteFaceSize), new Size());

			// each rectangle in faces is a face: draw them!
			return faces.toArray();
		}
		return new Rect[] {};

	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		camera.release();
		super.stop();
	}
}
