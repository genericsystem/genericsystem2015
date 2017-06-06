package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

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

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class FaceDetector extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	// private final static String imgClassDirectory = "classes/id-fr-front";
	private VideoCapture camera = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private CascadeClassifier faceCascade = new CascadeClassifier();

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

	public FaceDetector() {
		System.out.println(faceCascade.load("resources/haarcascade_frontalface_alt2.xml"));
		// System.out.println(faceCascade.load("/resources/haarcascade_frontalface_alt2.xml"));
	}

	public void detect(Mat frame) {

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
			this.faceCascade.detectMultiScale(grayFrame, faces, 1.1, 2, 0 | Objdetect.CASCADE_SCALE_IMAGE, new Size(absoluteFaceSize, absoluteFaceSize), new Size());

			// each rectangle in faces is a face: draw them!
			Rect[] facesArray = faces.toArray();
			for (int i = 0; i < facesArray.length; i++)
				Imgproc.rectangle(frame, facesArray[i].tl(), facesArray[i].br(), new Scalar(0, 255, 0), 3);
		}

	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		camera.release();
		super.stop();
	}
}
