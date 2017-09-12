package org.genericsystem.cv.docPattern;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.scene.layout.GridPane;

public class TestDocPattern extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		int rowIndex = 0;

		List<Img> images = new ArrayList();

		images.add(new Img("classes/id-fr-front/image5-0.png"));
		images.add(new Img("classes/id-fr-front/image-1.png"));
		images.add(new Img("classes/id-fr-front/image-3.png"));
		images.add(new Img("classes/id-fr-front/image14-0.png"));
		images.add(new Img("classes/id-fr-front/image2-0.png"));
		images.add(new Img("classes/id-fr-front/image6-0.png"));
		images.add(new Img("classes/id-fr-front/image4-0.png"));

		Img meanImg = imgMean(images);
		System.out.println(meanImg);
		mainGrid.add(meanImg.toJfxImageView(), 0, rowIndex++);
		mainGrid.add(preprocessing(meanImg).toJfxImageView(), 0, rowIndex++);

		List<ZoneStream> zoneStreams = new ArrayList<>();
		List<Rect> mergedRectangles = createZones(meanImg, 100, 5);
		for (Rect rect : mergedRectangles) {
			System.out.println("==================================================================================");
			List<String> labels = new ArrayList<>();
			for (Img image : images) {
				Img imgOcr = imgOcr(image, rect);
				String label = Ocr.doWork(imgOcr.getSrc());
				labels.add(label);
				Imgproc.putText(imgOcr.getSrc(),
						Normalizer.normalize(label, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""),
						new Point(10, 10), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
				mainGrid.add(imgOcr.toJfxImageView(), 0, rowIndex++);
				System.out.println(label);
			}
			ZoneStream zs = new ZoneStream(rect, labels);
			zoneStreams.add(zs);
			System.out.println("======>" + zs.greatestCommonPrefix());
			Imgproc.rectangle(meanImg.getSrc(), rect.tl(), rect.br(), new Scalar(255, 0, 0), 2);
		}
		DocPattern pattern = new DocPattern("id-fr-front", zoneStreams);
		mainGrid.add(meanImg.toJfxImageView(), 0, rowIndex++);

	}

	public Img imgMean(List<Img> images) {
		Mat mean = images.get(0).getSrc().clone();
		for (int i = 1; i < images.size(); i++) {
			double r = 1d / (i + 1d);
			Core.addWeighted(mean, 1 - r, images.get(i).getSrc(), r, 0, mean);
		}
		return new Img(mean);
	}

	public Img imgOcr(Img img, Rect rect) {
		double dW = Math.max(3, Math.min(1.1 * rect.height, 14)); // the font size is linked to the height of letters
		double dH = Math.max(3, Math.min(0.8 * rect.height, 14));
		double xtl = Math.max(0, rect.x - dW);
		double ytl = Math.max(0, rect.y - dH);
		double xbl = Math.min(img.width() - 1, rect.x + rect.width + dW);
		double ybl = Math.min(img.height() - 1, rect.y + rect.height + dH);
		return new Img(new Mat(img.getSrc(), new Rect(new Point(xtl, ytl), new Point(xbl, ybl)))).bilateralFilter(20,
				80, 80);
	}

	public List<Rect> createZones(Img img, int dW, int dH) {
		int minArea = 60;
		int minHeight = 6;
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(preprocessing(img).getSrc(), contours, new Mat(), Imgproc.RETR_LIST,
				Imgproc.CHAIN_APPROX_SIMPLE);
		List<Rect> rectangles = new ArrayList<>();
		contours.stream().filter(contour -> Imgproc.contourArea(contour, true) > minArea).map(Imgproc::boundingRect)
				.filter(rect -> rect.height >= minHeight).collect(Collectors.toList()).forEach(rect -> {
					rectangles.add(rect);
				});
		return mergeAllCloseRect(rectangles, dW, dH);
	}

	public Img preprocessing(Img img) {
		return img.adaptativeGaussianThreshold(17, 9)
				.morphologyEx(Imgproc.MORPH_OPEN, Imgproc.MORPH_ELLIPSE, new Size(40, 10)).bilateralFilter(20, 80, 80);
	}

	public boolean areClose(Rect rect1, Rect rect2, int dW, int dH) {

		// Returns true if rect1 and rect2 do overlap or almost (with intolerance dW and dH)

		// If one rectangle is on left side of the other one
		if (rect1.x > rect2.br().x + dW || rect2.x > rect1.br().x + dW)
			return false;

		// If one rectangle is above the other one
		if (rect1.y > rect2.br().y + dH || rect2.y > rect1.br().y + dH)
			return false;

		return true;

	}

	public Rect boundingRectangle(Rect rect1, Rect rect2) {

		// Returns the rectangle enclosing rect1 and rect2
		int xl = Math.min(rect1.x, rect2.x);
		int xr = (int) Math.max(rect1.br().x, rect2.br().x);
		int yt = Math.min(rect1.y, rect2.y);
		int yb = (int) Math.max(rect1.br().y, rect2.br().y);
		return new Rect(xl, yt, xr - xl, yb - yt);

	}

	public List<Rect> mergeAllCloseRect(List<Rect> rectangles, int dW, int dH) {
		int previousNumber = Integer.MAX_VALUE;
		while (previousNumber > rectangles.size()) {
			previousNumber = rectangles.size();
			rectangles = mergeCloseRect(rectangles, dW, dH);
		}
		return rectangles;
	}

	public List<Rect> mergeCloseRect(List<Rect> rectangles, int dW, int dH) {

		List<Rect> newRectangles = new ArrayList<>();
		List<Rect> merged = new ArrayList<>();
		for (Rect rect1 : rectangles) {
			if (!merged.contains(rect1)) {
				Rect newRect = rect1;
				for (Rect rect2 : rectangles) {
					if (!merged.contains(rect2) && newRect != rect2 && areClose(newRect, rect2, dW, dH)) {
						newRect = boundingRectangle(newRect, rect2);
						merged.add(rect1);
						merged.add(rect2);
					}
				}
				newRectangles.add(newRect);
			}
		}
		return newRectangles;
	}

}
