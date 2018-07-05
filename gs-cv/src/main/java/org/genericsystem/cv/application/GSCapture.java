package org.genericsystem.cv.application;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.PdfToPngConverter;
import org.opencv.core.Size;

public interface GSCapture {
	public static final Size HD = new Size(1280, 720);
	public static final Size VGA = new Size(640, 360);

	Img read();

	void release();

	Size getResize();

	public static class GSPhotoCapture implements GSCapture {

		private final Img img;

		public GSPhotoCapture(String url) {
			Img img;
			if (url.endsWith(".pdf"))
				img = PdfToPngConverter.imgFromPdf(url, 0);
			else
				img = new Img(url);
			double coeff = Math.min(VGA.width / img.width(), VGA.height / img.height());
			this.img = img.resize(new Size(coeff * img.width(), coeff * img.height()));
		}

		@Override
		public Img read() {
			return img;
		}

		@Override
		public void release() {
			img.getSrc().release();
		}

		@Override
		public Size getResize() {
			return img.size();
		}
	}

}