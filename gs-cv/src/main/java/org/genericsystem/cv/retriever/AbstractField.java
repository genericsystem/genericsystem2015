package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.genericsystem.cv.utils.OCRPlasty.Tuple;
import org.genericsystem.cv.utils.RectToolsMapper;
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
	private static final int OCR_CONFIDENCE_THRESH = 5;

	protected Rect rect;
	protected Point center;
	protected Map<String, Integer> labels;
	protected Optional<String> consolidated;
	protected double confidence;
	protected long attempts;

	protected int deadCounter;

	public AbstractField() {
		this(new Rect());
	}

	public AbstractField(Rect rect) {
		updateRect(rect);
		this.labels = new HashMap<>();
		this.consolidated = Optional.empty();
		this.attempts = 0;
		this.confidence = 0;
		this.deadCounter = 0;
	}

	public void merge(AbstractField field) {
		field.getLabels().entrySet().forEach(entry -> labels.merge(entry.getKey(), entry.getValue(), Integer::sum));
		attempts += field.getAttempts();
		deadCounter += field.getDeadCounter();
		if (consolidated.isPresent() || field.getConsolidated().isPresent())
			consolidateOcr(false);
	}

	void updateRect(Rect rect) {
		this.rect = rect;
		this.center = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
	}

	public void ocr(Img rootImg) {
		if (rootImg.getSrc().empty())
			return;

		Rect largeRect = getLargeRect(rootImg, 0.03, 0.1);
		if (largeRect.empty() || largeRect.width < 3 || largeRect.height < 3)
			return;
		// Prevent OpenCV assertion failure
		if (!(0 <= largeRect.x && 0 <= largeRect.y && largeRect.x + largeRect.width < rootImg.getSrc().cols() && largeRect.y + largeRect.height < rootImg.getSrc().rows()))
			return;
		Mat roi = new Mat(rootImg.getSrc(), largeRect);
		String ocr = Ocr.doWork(roi, OCR_CONFIDENCE_THRESH);
		if (!ocr.isEmpty()) {
			labels.merge(ocr, 1, Integer::sum);
			attempts++;
		}
		roi.release();
	}

	public void consolidateOcr(boolean force) {
		consolidateOcr(Integer.MAX_VALUE, force);
	}

	protected void consolidateOcr(int limit, boolean force) {
		int labelsSize = getLabelsSize();
		if (force || labelsSize >= MIN_SIZE_CONSOLIDATION) {
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

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		if (isOnDisplay(display)) {
			Point[] targets = getRectPointsWithHomography(homography);
			drawRect(display, targets, deadCounter == 0 ? color : new Scalar(0, 0, 255), thickness);
			drawText(display, targets, new Scalar(0, 64, 255), thickness);
		}
	}

	public void drawRect(Img stabilizedDisplay, Scalar color, int thickness) {
		drawRect(stabilizedDisplay, RectToolsMapper.decomposeClockwise(rect), color, thickness);
	}

	public void drawRect(Img display, Point[] targets, Scalar color, int thickness) {
		for (int i = 0; i < targets.length; ++i)
			Imgproc.line(display.getSrc(), targets[i], targets[(i + 1) % targets.length], color, thickness);
	}

	public void drawText(Img display, Point[] targets, Scalar color, int thickness) {
		consolidated.ifPresent(consolidated -> {
			String text = Normalizer.normalize(consolidated, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
			String conf = String.format("%.3f", confidence);
			// --- //
			Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
			double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
			Imgproc.line(display.getSrc(), new Point(topCenter.x, topCenter.y - 2), new Point(topCenter.x, topCenter.y - 20), color, 1);
			Imgproc.putText(display.getSrc(), text, new Point(topCenter.x - l, topCenter.y - 22), Core.FONT_HERSHEY_TRIPLEX, 0.45, color, 1);
			Imgproc.putText(display.getSrc(), conf, new Point(topCenter.x - l, topCenter.y - 12), Core.FONT_HERSHEY_TRIPLEX, 0.35, color.conj());
		});
	}

	protected Point[] getRectPointsWithHomography(Mat homography) {
		List<Point> points = Arrays.asList(RectToolsMapper.decomposeClockwise(rect));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
		return results.toArray();
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
		return RectToolsMapper.isOverlapping(this.rect, otherRect);
	}

	public boolean isOverlapping(AbstractField other) {
		return isOverlapping(other.getRect());
	}

	public boolean isIn(AbstractField other) {
		return RectToolsMapper.getInsider(rect, other.getRect()).map(r -> r.equals(rect) ? true : false).orElse(false);
	}

	public boolean overlapsMoreThanThresh(Rect otherRect, double overlapThreshold) {
		return RectToolsMapper.inclusiveArea(this.rect, otherRect) > overlapThreshold;
	}

	public boolean isClusteredWith(Rect otherRect, double epsilon) {
		return RectToolsMapper.isInCluster(this.rect, otherRect, epsilon);
	}

	public boolean overlapsMoreThanThresh(AbstractField other, double overlapThreshold) {
		return overlapsMoreThanThresh(other.getRect(), overlapThreshold);
	}

	public boolean isOnDisplay(Img display) {
		Rect imgRect = new Rect(0, 0, display.width(), display.height());
		return RectToolsMapper.isOverlapping(imgRect, this.rect);
	}

	public boolean isConsolidated() {
		return consolidated.isPresent();
	}

	public boolean needOcr() {
		return ThreadLocalRandom.current().nextBoolean();
	}

	public void incrementDeadCounter() {
		deadCounter++;
	}

	public void resetDeadCounter() {
		deadCounter = 0;
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

	public int getDeadCounter() {
		return deadCounter;
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("AbstractField: ").append("\n").append(" -> rect: ").append(rect).append("\n").append(" -> labels size: ").append(labels.size()).append("\n").append(" -> consolidated: ").append(consolidated).append("\n").append(" -> confidence: ")
				.append(confidence).append("\n");
		return sb.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((center == null) ? 0 : center.hashCode());
		long temp;
		temp = Double.doubleToLongBits(confidence);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((consolidated == null) ? 0 : consolidated.hashCode());
		result = prime * result + ((labels == null) ? 0 : labels.hashCode());
		result = prime * result + ((rect == null) ? 0 : rect.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractField other = (AbstractField) obj;
		if (center == null) {
			if (other.center != null)
				return false;
		} else if (!center.equals(other.center))
			return false;
		if (Double.doubleToLongBits(confidence) != Double.doubleToLongBits(other.confidence))
			return false;
		if (consolidated == null) {
			if (other.consolidated != null)
				return false;
		} else if (!consolidated.equals(other.consolidated))
			return false;
		if (labels == null) {
			if (other.labels != null)
				return false;
		} else if (!labels.equals(other.labels))
			return false;
		if (rect == null) {
			if (other.rect != null)
				return false;
		} else if (!rect.equals(other.rect))
			return false;
		return true;
	}

}
