package org.genericsystem.cv.application;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;
import org.opencv.videoio.Videoio;

public class GSVideoCapture implements GSCapture {

	private final VideoCapture videoCapture;

	private final Size size;
	private final Size resize;

	GSVideoCapture(String url, Size size, Size resize) {
		this.size = size;
		this.resize = resize;
		videoCapture = new VideoCapture(url);
		videoCapture.set(Videoio.CAP_PROP_FRAME_WIDTH, size.width);
		videoCapture.set(Videoio.CAP_PROP_FRAME_HEIGHT, size.height);
	}

	GSVideoCapture(int index, Size size, Size resize) {
		this.size = size;
		this.resize = resize;
		videoCapture = new VideoCapture(index);
		videoCapture.set(Videoio.CAP_PROP_FRAME_WIDTH, size.width);
		videoCapture.set(Videoio.CAP_PROP_FRAME_HEIGHT, size.height);
	}

	@Override
	public Size getResize() {
		return resize;
	}

	@Override
	public Img read() {
		Mat frameMat = new Mat();
		boolean result = videoCapture.read(frameMat);
		if (!result)
			throw new IllegalStateException("Unable to read camera");
		Imgproc.resize(frameMat, frameMat, resize, 1, 1, Imgproc.INTER_LINEAR);
		return new Img(frameMat, false);
	}

	@Override
	public void release() {
		videoCapture.release();
	}

}