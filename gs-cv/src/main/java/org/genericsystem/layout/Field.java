package org.genericsystem.layout;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Field {
	private final Rect rect;
	private Map<String, Integer> labels = new HashMap<>();
	private String consolidated;
	private int attempts = 0;

	public Field(Rect rect) {
		this.rect = rect;
	}

	public void merge(Field field) {
		labels = field.getLabels();
		consolidated = field.getConsolidated();
	}

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public boolean contains(Point center) {
		Point localCenter = center();
		return Math.sqrt(Math.pow(localCenter.x - center.x, 2) + Math.pow(localCenter.y - center.y, 2)) <= 10;
	}

	public Point center() {
		return new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(center())), results, homography);
		Point[] targets = results.toArray();
		Imgproc.line(display.getSrc(), targets[0], new Point(targets[0].x, targets[0].y - 30), color, thickness);
		Imgproc.putText(display.getSrc(), Normalizer.normalize(consolidated, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), new Point(targets[0].x - 10, targets[0].y - 30), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
	}

	public void draw(Img stabilizedDisplay) {
		Imgproc.rectangle(stabilizedDisplay.getSrc(), rect.tl(), rect.br(), new Scalar(0, 0, 255));
	}

	public void ocr(Img rootImg) {
		String ocr = Ocr.doWork(new Mat(rootImg.getSrc(), getLargeRect(rootImg, 0.03, 0.1)));
		Integer count = labels.get(ocr);
		labels.put(ocr, 1 + (count != null ? count : 0));
		int all = labels.values().stream().reduce(0, (i, j) -> i + j);
		for (Entry<String, Integer> entry : labels.entrySet())
			if (entry.getValue() > all / 2)
				consolidated = entry.getKey();
		attempts++;
	}

	public Rect getLargeRect(Img imgRoot, double deltaW, double deltaH) {
		int adjustW = 3 + Double.valueOf(Math.floor(rect.width * deltaW)).intValue();
		int adjustH = 3 + Double.valueOf(Math.floor(rect.height * deltaH)).intValue();

		Point tl = new Point(rect.tl().x - adjustW > 0 ? rect.tl().x - adjustW : 0, rect.tl().y - adjustH > 0 ? rect.tl().y - adjustH : 0);
		Point br = new Point(rect.br().x + adjustW > imgRoot.width() ? imgRoot.width() : rect.br().x + adjustW, rect.br().y + adjustH > imgRoot.height() ? imgRoot.height() : rect.br().y + adjustH);

		return new Rect(tl, br);
	}

	public boolean isConsolidated() {
		return consolidated != null && consolidated.length() >= 5;
	}

	public String getConsolidated() {
		return consolidated;
	}

	public boolean needOcr() {
		return consolidated == null && attempts < 10;
	}

}