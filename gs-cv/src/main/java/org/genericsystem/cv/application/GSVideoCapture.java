package org.genericsystem.cv.application;

import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;
import org.opencv.videoio.Videoio;

public class GSVideoCapture implements GSCapture {
	public static final Size HD = new Size(1280, 1024);
	public static final Size VGA = new Size(640, 480);
	private final VideoCapture videoCapture;
	private final double f;

	private final Size size;
	private final Size resize;

	GSVideoCapture(String url, double f, Size size, Size resize) {
		this.f = f;
		this.size = size;
		this.resize = resize;
		videoCapture = new VideoCapture(url);
		videoCapture.set(Videoio.CAP_PROP_FRAME_WIDTH, size.width);
		videoCapture.set(Videoio.CAP_PROP_FRAME_HEIGHT, size.height);
	}

	GSVideoCapture(int index, double f, Size size, Size resize) {
		this.f = f;
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
	public SuperFrameImg read() {
		Mat frameMat = new Mat();
		videoCapture.read(frameMat);
		Imgproc.resize(frameMat, frameMat, resize);
		return new SuperFrameImg(frameMat, new double[] { frameMat.width() / 2, frameMat.height() / 2 }, f);
	}

	@Override
	public void release() {
		videoCapture.release();
	}

}