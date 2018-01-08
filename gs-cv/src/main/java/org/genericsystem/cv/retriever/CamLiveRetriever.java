package org.genericsystem.cv.retriever;


import org.opencv.core.Mat;
import org.opencv.videoio.VideoCapture;

public class CamLiveRetriever extends LiveRetrieverBase {

	private final VideoCapture capture = new VideoCapture(0);
	private Mat frame = new Mat();

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public Mat updateFrame() {
		capture.read(frame);
		return frame;
	}
}
