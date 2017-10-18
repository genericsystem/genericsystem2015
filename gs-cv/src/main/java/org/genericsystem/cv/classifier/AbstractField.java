package org.genericsystem.cv.classifier;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.RectangleTools;
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
	protected Optional<String> consolidated;
	protected long attempts;

	public AbstractField(Rect rect) {
		this.rect = rect;
		this.labels = new HashMap<>();
		this.consolidated = Optional.empty();
		this.center = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
		this.attempts = 0;
	}

	public void merge(AbstractField field) {
		field.getLabels().entrySet().forEach(entry -> labels.merge(entry.getKey(), entry.getValue(), Integer::sum));
		consolidated = field.getConsolidated(); // TODO: attempt to merge 2 optionals, or compute again the consolidated text from the labels
		attempts += field.getAttempts();
	}

	public void merge(List<AbstractField> fields) {
		fields.forEach(f -> this.merge(f));
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
			Imgproc.putText(display.getSrc(), Normalizer.normalize(consolidated.orElse(""), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), new Point(topCenter.x - l, topCenter.y - 22), Core.FONT_HERSHEY_TRIPLEX, 0.45, new Scalar(0, 255, 0), 1);
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

	public boolean isOverlapping(AbstractField other) {
		return RectangleTools.isOverlapping(this.rect, other.getRect());
	}

	public boolean overlapsMoreThanThresh(AbstractField other, double overlapThreshold) {
		double[] res = RectangleTools.commonArea(this.rect, other.getRect());
		return res[0] > overlapThreshold;
	}

	public boolean isOnDisplay(Img display) {
		Point[] points = RectangleTools.decomposeClockwise(rect);
		boolean ok = false;
		// True if one of the 4 corners is inside the image
		for (int i = 0; i < points.length; ++i) {
			ok = ok || (points[i].x > 0 && points[i].x < display.width() && points[i].y > 0 && points[i].y < display.height());
		}
		if (!ok) {
			// True if the rectangle is bigger than display
			ok = ok || (rect.tl().x < 0 && rect.br().x > display.width() && rect.tl().y < 0 && rect.br().y > display.height());
			// if (!ok) {
			// Deal with the case where the bottom/top/left/right of the rect is inside the image
			// }
		}
		return ok;
	}

	public boolean needMoreAttempts() {
		return getLabelsSize() < 20;
	}

	public boolean isConsolidated() {
		return consolidated.isPresent();
	}

	public boolean needOcr() {
		return !isConsolidated() || needMoreAttempts();
	}

	public boolean canBeOCR(Img display) {
		Point[] points = RectangleTools.decomposeClockwise(rect);
		for (int i = 0; i < points.length; ++i) {
			if (points[i].x < 0 || points[i].y < 0 || points[i].x > display.width() || points[i].y > display.height())
				return false;
		}
		return true;
	}

	public abstract void ocr(Img rootImg);

	protected abstract void consolidateOcr();

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public int getLabelsSize() {
		return labels.entrySet().stream().mapToInt(entry -> entry.getValue()).sum();
	}

	public Optional<String> getConsolidated() {
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
