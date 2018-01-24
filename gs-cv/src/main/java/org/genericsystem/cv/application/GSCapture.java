package org.genericsystem.cv.application;

import org.genericsystem.cv.Img;
import org.opencv.core.Size;

interface GSCapture {

	SuperFrameImg read();

	void release();

	Size getResize();

	public static class GSPhotoCapture implements GSCapture {

		private final SuperFrameImg superFrame;

		public GSPhotoCapture(String url, double f) {
			Img img = new Img(url);
			superFrame = new SuperFrameImg(img.getSrc(), new double[] { img.width() / 2, img.height() / 2 }, f);
		}

		@Override
		public SuperFrameImg read() {
			return superFrame;
		}

		@Override
		public void release() {
			superFrame.getFrame().getSrc().release();
		}

		@Override
		public Size getResize() {
			return superFrame.size();
		}
	}

}