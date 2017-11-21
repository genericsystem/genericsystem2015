package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.IntStream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.genericsystem.cv.utils.OCRPlasty.Tuple;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
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
	private static final int OCR_CONFIDENCE_THRESH = 0;

	protected GSRect rect;
	protected Map<String, Integer> labels;
	protected String consolidated;
	protected double confidence;
	protected long attempts;

	protected int deadCounter;

	public AbstractField() {
		this(new GSRect());
	}

	public AbstractField(GSRect rect) {
		this.rect = rect;
		this.labels = new HashMap<>();
		this.consolidated = null;
		this.attempts = 0;
		this.confidence = 0;
		this.deadCounter = 0;
	}

	public AbstractField(AbstractField other) {
		this.rect = other.getRect();
		this.labels = other.getLabels();
		this.consolidated = other.getConsolidated();
		this.attempts = other.getAttempts();
		this.confidence = other.getConfidence();
		this.deadCounter = other.getDeadCounter();
	}

	void updateRect(GSRect rect) {
		this.rect = rect;
	}

	public void ocr(Img rootImg) {
		if (rootImg.getSrc().empty() || rootImg.getSrc().width() <= 3 || rootImg.getSrc().height() <= 3)
			return;
		Rect ocrRect = new Rect((int) rect.getX(), (int) rect.getY(), (int) rect.getWidth(), (int) rect.getHeight());
		if (ocrRect.empty() || ocrRect.width <= 3 || ocrRect.height <= 3)
			return;
		// Prevent OpenCV assertion failure
		if (!(0 <= ocrRect.x && 0 <= ocrRect.y && ocrRect.x + ocrRect.width < rootImg.getSrc().cols() && ocrRect.y + ocrRect.height < rootImg.getSrc().rows()))
			return;
		Mat roi = new Mat(rootImg.getSrc(), ocrRect);
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
			this.consolidated = res.getString().orElse(null);
			this.confidence = res.getConfidence();

			if (labelsSize >= 2 * MIN_SIZE_CONSOLIDATION)
				res.getOutliers().forEach(outlier -> labels.remove(outlier));
		} else {
			logger.trace("Not enough labels to consolidate OCR (current minimum = {})", MIN_SIZE_CONSOLIDATION);
			this.consolidated = null;
			this.confidence = 0;
		}
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, int thickness) {
		Point[] targets = getRectPointsWithHomography(homography);
		drawRect(display, targets, deadCounter == 0 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
		drawText(display, targets, deadCounter == 0 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
	}

	public void drawRect(Img stabilizedDisplay, Scalar color, int thickness) {
		Point[] points = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
		drawRect(stabilizedDisplay, points, color, thickness);
	}

	public void drawRect(Img display, Point[] targets, Scalar color, int thickness) {
		for (int i = 0; i < targets.length; ++i)
			Imgproc.line(display.getSrc(), targets[i], targets[(i + 1) % targets.length], color, thickness);
	}

	public void drawText(Img display, Scalar color, int thickness) {
		Point[] points = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
		drawText(display, points, color, thickness);
	}

	public void drawText(Img display, Point[] targets, Scalar color, int thickness) {
		if (consolidated != null) {
			String text = Normalizer.normalize(consolidated, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
			// --- //
			Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
			double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
			Imgproc.line(display.getSrc(), new Point(topCenter.x, topCenter.y - 2), new Point(topCenter.x, topCenter.y - 12), color, 1);
			Imgproc.putText(display.getSrc(), text, new Point(topCenter.x - l, topCenter.y - 14), Core.FONT_HERSHEY_TRIPLEX, 0.45, color, 1);
		}
	}

	public void drawDebugText(Img display, Scalar color, int thickness) {
		Point[] points = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
		drawDebugText(display, points, color, thickness);
	}

	public void drawDebugText(Img display, Point[] targets, Scalar color, int thickness) {
		String conf = String.format("%.3f", confidence);
		Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
		double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
		Imgproc.putText(display.getSrc(), conf, new Point(topCenter.x - l, topCenter.y - 12), Core.FONT_HERSHEY_TRIPLEX, 0.35, color);
	}

	protected Point[] getRectPointsWithHomography(Mat homography) {
		List<Point> points = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise()));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
		return results.toArray();
	}

	public Rect getLargeRect(Img imgRoot, double deltaW, double deltaH) {
		int adjustW = 3 + Double.valueOf(Math.floor(rect.getWidth() * deltaW)).intValue();
		int adjustH = 3 + Double.valueOf(Math.floor(rect.getHeight() * deltaH)).intValue();
		Point tl = new Point(rect.tl().getX() - adjustW > 0 ? rect.tl().getX() - adjustW : 0, rect.tl().getY() - adjustH > 0 ? rect.tl().getY() - adjustH : 0);
		Point br = new Point(rect.br().getX() + adjustW > imgRoot.width() ? imgRoot.width() : rect.br().getX() + adjustW, rect.br().getY() + adjustH > imgRoot.height() ? imgRoot.height() : rect.br().getY() + adjustH);
		return new Rect(tl, br);
	}

	public boolean isOverlapping(GSRect otherRect) {
		return this.rect.isOverlapping(otherRect);
	}

	public boolean overlapsMoreThanThresh(GSRect otherRect, double overlapThreshold) {
		return this.rect.inclusiveArea(otherRect) > overlapThreshold;
	}

	public boolean isClusteredWith(GSRect otherRect, double epsilon) {
		return RectangleTools.isInCluster(this.rect, otherRect, epsilon);
	}

	public boolean isClusteredWith(GSRect otherRect, double epsilon, int sides) {
		return RectangleTools.isInCluster(this.rect, otherRect, epsilon, sides);
	}

	public boolean isConsolidated() {
		return consolidated != null;
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

	public String getConsolidated() {
		return consolidated;
	}

	public long getAttempts() {
		return attempts;
	}

	public GSRect getRect() {
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
		sb.append("AbstractField: ").append("\n").append(" -> rect: ").append(rect).append("\n").append(" -> labels size: ").append(getLabelsSize()).append("\n").append(" -> consolidated: ").append(consolidated).append("\n").append(" -> confidence: ")
				.append(confidence).append("\n");
		return sb.toString();
	}

}
