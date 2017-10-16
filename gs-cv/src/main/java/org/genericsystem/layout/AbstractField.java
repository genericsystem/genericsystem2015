package org.genericsystem.layout;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public abstract class AbstractField {
	protected final Rect rect;
	protected final Point center;
	protected Map<String, Integer> labels;
	protected String consolidated;
	protected long attempts;

	public AbstractField(Rect rect) {
		this.rect = rect;
		this.labels = new HashMap<>();
		this.center = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
		this.attempts = 0;
	}

	public void draw(Img stabilizedDisplay) {
		Imgproc.rectangle(stabilizedDisplay.getSrc(), rect.tl(), rect.br(), new Scalar(0, 0, 255));
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		if (isOnDisplay(display)) {
			List<Point> points = Arrays.asList(center, new Point(rect.x, rect.y), new Point(rect.x + rect.width - 1, rect.y), new Point(rect.x + rect.width - 1, rect.y + rect.height - 1), new Point(rect.x, rect.y + rect.height - 1));
			MatOfPoint2f results = new MatOfPoint2f();
			Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
			Point[] targets = results.toArray();
			Imgproc.line(display.getSrc(), targets[1], targets[2], color, thickness);
			Imgproc.line(display.getSrc(), targets[2], targets[3], color, thickness);
			Imgproc.line(display.getSrc(), targets[3], targets[4], color, thickness);
			Imgproc.line(display.getSrc(), targets[4], targets[1], color, thickness);
			Point topCenter = new Point((targets[1].x + targets[2].x) / 2, (targets[1].y + targets[2].y) / 2);
			double l = Math.sqrt(Math.pow(targets[1].x - topCenter.x, 2) + Math.pow(targets[1].y - topCenter.y, 2));
			Imgproc.line(display.getSrc(), new Point(topCenter.x, topCenter.y - 2), new Point(topCenter.x, topCenter.y - 20), new Scalar(0, 255, 0), 1);
			Imgproc.putText(display.getSrc(), Normalizer.normalize(consolidated, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), new Point(topCenter.x - l, topCenter.y - 22), Core.FONT_HERSHEY_TRIPLEX, 0.45, new Scalar(0, 255, 0), 1);
		}
	}

	public Rect getLargeRect(Img imgRoot, double deltaW, double deltaH) {
		int adjustW = 3 + Double.valueOf(Math.floor(rect.width * deltaW)).intValue();
		int adjustH = 3 + Double.valueOf(Math.floor(rect.height * deltaH)).intValue();

		Point tl = new Point(rect.tl().x - adjustW > 0 ? rect.tl().x - adjustW : 0, rect.tl().y - adjustH > 0 ? rect.tl().y - adjustH : 0);
		Point br = new Point(rect.br().x + adjustW > imgRoot.width() ? imgRoot.width() : rect.br().x + adjustW, rect.br().y + adjustH > imgRoot.height() ? imgRoot.height() : rect.br().y + adjustH);

		return new Rect(tl, br);
	}

	public boolean contains(Point center) {
		return Math.sqrt(Math.pow(this.center.x - center.x, 2) + Math.pow(this.center.y - center.y, 2)) <= 10;
	}

	public boolean isOnDisplay(Img display) {
		return rect.br().x > 0 && rect.br().y > 0;
	}

	public boolean needMoreAttempts() {
		return getLabelsSize() < 20;
	}

	public boolean isConsolidated() {
		return consolidated != null;
	}

	public boolean needOcr() {
		return !isConsolidated() || needMoreAttempts();
	}

	public abstract void ocr(Img rootImg);

	protected abstract void consolidateOcr();

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public int getLabelsSize() {
		return labels.entrySet().stream().mapToInt(entry -> entry.getValue()).sum();
	}

	public String getConsolidated() {
		return consolidated;
	}

	public long getAttempts() {
		return attempts;
	}

	public Point getCenter() {
		return center;
	}

	public Rect getRect() {
		return rect;
	}

}
