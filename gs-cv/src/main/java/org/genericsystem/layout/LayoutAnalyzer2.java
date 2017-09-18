package org.genericsystem.layout;

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

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class LayoutAnalyzer2 extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;
		rowIndex = 0;
		columnIndex++;

		final String filename = "resources/document2.png";
		Img img = new Img(filename);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);

		Img img2 = new Img(filename).bilateralFilter(20, 80, 80);
		Img closed = img2.adaptativeGaussianThreshold(17, 9).morphologyEx(Imgproc.MORPH_OPEN, Imgproc.MORPH_ELLIPSE,
				new Size(40, 10));
		mainGrid.add(new ImageView(closed.toJfxImage()), columnIndex, rowIndex++);

		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 0;
		double minHeight = 10;
		List<Rect> rectangles = new ArrayList<>();
		contours.stream().filter(contour -> Imgproc.contourArea(contour, true) > minArea).map(Imgproc::boundingRect)
				.filter(rect -> rect.height >= minHeight).collect(Collectors.toList()).forEach(rect -> {
					rectangles.add(rect);
					// Imgproc.rectangle(img2.getSrc(), rect.tl(), rect.br(), new Scalar(0, 0, 0), 2);
				});

		List<Rect> mergedRectangles = mergeAllOverlaps(rectangles);
		for (Rect rect : mergedRectangles) {
			Imgproc.putText(img2.getSrc(),
					Normalizer.normalize(rectOcr(img2, rect), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""),
					rect.tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
			Imgproc.rectangle(img2.getSrc(), rect.tl(), rect.br(), new Scalar(255, 0, 0), 2);
		}
		mainGrid.add(new ImageView(img2.toJfxImage()), columnIndex, rowIndex++);

		Img img3 = new Img(filename).bilateralFilter(20, 80, 80);
		Layout layout = closed.buildLayout();
		layout.draw(img3, new Scalar(0, 255, 0), 2);
		mainGrid.add(new ImageView(img3.toJfxImage()), columnIndex, rowIndex++);

		// Close the images
		img.close();
		img2.close();
	}

	public String rectOcr(Img img, Rect rect) {
		double dW = 0.05;
		double dH = 0.05;
		double xtl = Math.max(0, rect.x - rect.height * dW); // the font size is linked to the height of letters
		double ytl = Math.max(0, rect.y - rect.height * dH);
		double xbl = Math.min(img.width() - 1, rect.x + rect.width + rect.width * dW);
		double ybl = Math.min(img.height() - 1, rect.y + rect.height + rect.height * dH);
		return Ocr.doWork(new Mat(img.getSrc(), new Rect(new Point(xtl, ytl), new Point(xbl, ybl))));
	}

	public boolean doAlmostOverlap(Rect rect1, Rect rect2) {

		// Returns true if rect1 and rect2 do "almost" overlap (with intolerance dW and dH)

		int dW = 20;
		int dH = 5;

		// If one rectangle is on left side of other
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

	public List<Rect> mergeAllOverlaps(List<Rect> rectangles) {
		int previousNumber = Integer.MAX_VALUE;
		while (previousNumber > rectangles.size()) {
			previousNumber = rectangles.size();
			rectangles = mergeOverlap(rectangles);
		}
		return rectangles;
	}

	public List<Rect> mergeOverlap(List<Rect> rectangles) {

		List<Rect> newRectangles = new ArrayList<>();
		List<Rect> merged = new ArrayList<>();
		for (Rect rect1 : rectangles) {
			if (!merged.contains(rect1)) {
				Rect newRect = rect1;
				for (Rect rect2 : rectangles) {
					if (!merged.contains(rect2) && newRect != rect2 && doAlmostOverlap(newRect, rect2)) {
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
