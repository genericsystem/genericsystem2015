package org.genericsystem.cv.retriever;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

public class RetrieverFromImg extends LiveRetrieverBase {

	private Mat frame = new Img(Imgcodecs.imread("classes/facture/factures-2.png")).resize(640).getSrc();

	public RetrieverFromImg() {
		mode = DeperspectivationMode.NONE;
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public Mat updateFrame() {
		return new Img(frame).getSrc();
	}
}
