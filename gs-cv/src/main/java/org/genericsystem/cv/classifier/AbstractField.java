package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.IntStream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.genericsystem.cv.utils.OCRPlasty.Tuple;
import org.genericsystem.cv.utils.RectangleTools;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractField {

	protected static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	protected static final int MIN_SIZE_CONSOLIDATION = 5;

	protected final Rect rect;
	protected final Point center;
	protected Map<String, Integer> labels;
	protected Optional<String> consolidated;
	protected double confidence;
	protected long attempts;

	public AbstractField(Rect rect) {
		this.rect = rect;
		this.labels = new HashMap<>();
		this.consolidated = Optional.empty();
		this.center = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
		this.attempts = 0;
		this.confidence = 0;
	}

	public void merge(AbstractField field) {
		field.getLabels().entrySet().forEach(entry -> labels.merge(entry.getKey(), entry.getValue(), Integer::sum));
		attempts += field.getAttempts();
		consolidateOcr();
	}

	public void merge(List<AbstractField> fields) {
		fields.forEach(f -> this.merge(f));
	}

	public void ocr(Img rootImg) {
		Rect largeRect = getLargeRect(rootImg, 0.03, 0.1);
		if (largeRect.empty() || largeRect.width < 3 || largeRect.height < 3)
			return;
		// Prevent OpenCV assertion failure
		if (!(0 <= largeRect.y && largeRect.y <= largeRect.y + largeRect.height && largeRect.y + largeRect.height <= rootImg.getSrc().rows()))
			return;
		Mat roi = new Mat(rootImg.getSrc(), largeRect);
		String ocr = Ocr.doWork(roi);
		if (!ocr.isEmpty()) {
			labels.merge(ocr, 1, Integer::sum);
			attempts++;
		}
		roi.release();
	}

	protected void consolidateOcr() {
		consolidateOcr(Integer.MAX_VALUE);
	}

	protected void consolidateOcr(int limit) {
		int labelsSize = getLabelsSize();
		if (labelsSize >= MIN_SIZE_CONSOLIDATION) {
			List<String> strings;
			if (Integer.MAX_VALUE == limit)
				strings = labels.entrySet().stream().collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())), List::addAll);
			else
				strings = labels.entrySet().stream().sorted(Entry.<String, Integer>comparingByValue().reversed()).limit(limit).collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())),
						List::addAll);
			Tuple res = OCRPlasty.correctStringsAndGetOutliers(strings, RANSAC.NORM_LEVENSHTEIN);
			this.consolidated = res.getString();
			this.confidence = res.getConfidence();

			if (labelsSize >= 2 * MIN_SIZE_CONSOLIDATION)
				res.getOutliers().forEach(outlier -> labels.remove(outlier));
		} else {
			logger.trace("Not enough labels to consolidate OCR (current minimum = {})", MIN_SIZE_CONSOLIDATION);
			this.consolidated = Optional.empty();
			this.confidence = 0;
		}
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

	public boolean isOverlapping(Rect otherRect) {
		return RectangleTools.isOverlapping(this.rect, otherRect);
	}

	public boolean isOverlapping(AbstractField other) {
		return isOverlapping(other.getRect());
	}

	public boolean isIn(AbstractField other) {
		return RectangleTools.getInsider(rect, other.getRect()).map(r -> r.equals(rect) ? true : false).orElse(false);
	}

	public boolean overlapsMoreThanThresh(Rect otherRect, double overlapThreshold) {
		return RectangleTools.inclusiveArea(this.rect, otherRect) > overlapThreshold;
	}

	public boolean overlapsMoreThanThresh(AbstractField other, double overlapThreshold) {
		return overlapsMoreThanThresh(other.getRect(), overlapThreshold);
	}

	public boolean isOnDisplay(Img display) {
		Rect imgRect = new Rect(0, 0, display.width(), display.height());
		return RectangleTools.isOverlapping(imgRect, this.rect);
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

	public int getLabelsSize() {
		return labels.entrySet().stream().mapToInt(entry -> entry.getValue()).sum();
	}

	public Map<String, Integer> getLabels() {
		return labels;
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

	public double getConfidence() {
		return confidence;
	}

}
