package org.genericsystem.cv.application;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.PdfToPngConverter;
import org.opencv.core.Size;

interface GSCapture {
	public static final Size HD = new Size(1280, 720);
	public static final Size VGA = new Size(320, 160);

	SuperFrameImg read();

	void release();

	Size getResize();

	public static class GSPhotoCapture implements GSCapture {

		private final SuperFrameImg superFrame;

		public GSPhotoCapture(String url, double f) {
			Img img;
			if (url.endsWith(".pdf"))
				img = PdfToPngConverter.imgFromPdf(url, 0);
			else
				img = new Img(url);
			double coeff = Math.min(VGA.width / img.width(), VGA.height / img.height());
			img = img.resize(new Size(coeff * img.width(), coeff * img.height()));
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