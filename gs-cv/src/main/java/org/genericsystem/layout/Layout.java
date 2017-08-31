package org.genericsystem.layout;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Layout {
	// Golden ratio
	private static final Double gold = (1 + Math.sqrt(5)) / 2;

	private double x1;
	private double x2;
	private double y1;
	private double y2;

	private Map<String, Integer> labels = new HashMap<>();
	private List<Layout> children = new ArrayList<>();
	private Layout parent = null;

	public Layout(Layout parent, double x1, double x2, double y1, double y2) {
		this.parent = parent;
		this.x1 = x1;
		this.x2 = x2;
		this.y1 = y1;
		this.y2 = y2;
	}

	public Img getRoi(Img img) {
		return new Img(img, this);
	}

	public Rect getRect(Img imgRoot) {
		Rect parentRect = getParent() != null ? getParent().getRect(imgRoot) : new Rect(0, 0, imgRoot.width(), imgRoot.height());
		return new Rect(new Point(parentRect.tl().x + parentRect.width * getX1(), parentRect.tl().y + parentRect.height * getY1()), new Point(parentRect.tl().x + parentRect.width * getX2(), parentRect.tl().y + parentRect.height * getY2()));
	}

	public double area(Img imgRoot) {
		double result = 0;
		if (this.getChildren().isEmpty()) {
			result += getRect(imgRoot).area() / (imgRoot.height() * imgRoot.width());
		} else {
			for (Layout child : getChildren()) {
				result += child.area(imgRoot);
			}
		}
		return result;
	}

	public Rect getLargeRect(Img imgRoot, int delta) {
		Rect rect = getRect(imgRoot);
		return new Rect(new Point(rect.tl().x - delta >= 0 ? rect.tl().x : 0, rect.tl().y - delta >= 0 ? rect.tl().y : 0),
				new Point(rect.br().x + delta <= imgRoot.width() ? rect.br().x + delta : imgRoot.width(), rect.br().y + delta <= imgRoot.height() ? rect.br().y + delta : imgRoot.height()));
	}

	public Layout traverse(Img img, BiConsumer<Img, Layout> visitor) {
		for (Layout shard : getChildren())
			shard.traverse(shard.getRoi(img), visitor);
		visitor.accept(img, this);
		return this;
	}

	public void draw(Img img, Scalar color, int thickness) {
		traverse(getRoi(img), (roi, shard) -> {
			if (shard.getChildren().isEmpty())
				Imgproc.rectangle(roi.getSrc(), new Point(0, 0), new Point(roi.width() - 1, roi.height() - 1), color, thickness);
			else
				Imgproc.rectangle(roi.getSrc(), new Point(0, 0), new Point(roi.width() - 1, roi.height() - 1), new Scalar(0, 0, 255), thickness);
		});

	}

	public void drawPerspective(Img img, Mat homography, Scalar color, int thickness) {
		traverse(getRoi(img), (roi, shard) -> {
			if (shard.getChildren().isEmpty()) {
				MatOfPoint2f results = new MatOfPoint2f();
				Rect rect = shard.getRect(img);
				List<Point> points = Arrays.asList(new Point(rect.tl().x, rect.tl().y), new Point(rect.tl().x + roi.width() - 1, rect.tl().y), new Point(rect.tl().x + roi.width() - 1, rect.tl().y + roi.height() - 1),
						new Point(rect.tl().x, rect.tl().y + roi.height() - 1));
				Mat pts = Converters.vector_Point2f_to_Mat(points);
				Core.perspectiveTransform(pts, results, homography);
				Point[] targets = results.toArray();
				Imgproc.line(img.getSrc(), targets[0], targets[1], color, thickness);
				Imgproc.line(img.getSrc(), targets[1], targets[2], color, thickness);
				Imgproc.line(img.getSrc(), targets[2], targets[3], color, thickness);
				Imgproc.line(img.getSrc(), targets[3], targets[0], color, thickness);
			}
		});
	}

	public void ocrTree(Img rootImg, int delta) {
		traverse(rootImg, (root, layout) -> {
			if (layout.getChildren().isEmpty()) {
				String ocr = Ocr.doWork(new Mat(rootImg.getSrc(), layout.getLargeRect(rootImg, delta)));
				if (!"".equals(ocr)) {
					Integer count = layout.getLabels().get(ocr);
					layout.getLabels().put(ocr, 1 + (count != null ? count : 0));
					int all = layout.getLabels().values().stream().reduce(0, (i, j) -> i + j);
					layout.getLabels().entrySet().forEach(entry -> {
						if (entry.getValue() > all / 10)
							Imgproc.putText(rootImg.getSrc(), Normalizer.normalize(entry.getKey(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), layout.getRect(rootImg).tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
					});
					// Imgproc.putText(rootImg.getSrc(), layout.getBestLabel(), layout.getRect(rootImg).tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
					// System.out.println(layout.getBestLabel());
				}
			}
		});
	}

	private String getBestLabel() {
		String result = "";
		int occurence = 0;
		for (String key : getLabels().keySet()) {
			if (getLabels().get(key) > occurence) {
				result = key;
				occurence = getLabels().get(key);
			}
		}
		return result;
	}

	public void addChild(Layout child) {
		if (!children.contains(child))
			children.add(child);
	}

	public void removeChild(Layout child) {
		if (children.contains(child))
			children.remove(child);
	}

	public boolean equiv(Layout s, double xTolerance, double yTolerance) {

		if (Math.abs(s.x1 - x1) <= xTolerance && Math.abs(s.x2 - x2) <= xTolerance && Math.abs(s.y1 - y1) <= yTolerance && Math.abs(s.y2 - y2) <= yTolerance)
			return true;

		return false;
	}

	public List<Layout> getChildren() {
		return children;
	}

	public double getX1() {
		return x1;
	}

	public void setX1(double x1) {
		this.x1 = x1;
	}

	public double getX2() {
		return x2;
	}

	public void setX2(double x2) {
		this.x2 = x2;
	}

	public double getY1() {
		return y1;
	}

	public void setY1(double y1) {
		this.y1 = y1;
	}

	public double getY2() {
		return y2;
	}

	public void setY2(double y2) {
		this.y2 = y2;
	}

	public Layout getParent() {
		return parent;
	}

	public void setParent(Layout parent) {
		this.parent = parent;
	}

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public boolean hasChildren() {
		return !getChildren().isEmpty();
	}

	public String recursiveToString() {
		StringBuilder sb = new StringBuilder();
		recursivToString(this, sb, 0);
		sb.append("\n");
		return sb.toString();
	}

	private void recursivToString(Layout shard, StringBuilder sb, int depth) {
		sb.append("depth : " + depth + " : ");
		sb.append("[(" + shard.x1 + "," + shard.x2 + "),(" + shard.y1 + "," + shard.y2 + ")]".toString());
		if (!shard.hasChildren())
			sb.append(" : Labels : " + shard.labels);
		else {
			depth++;
			for (Layout s : shard.getChildren()) {
				sb.append("\n");
				for (int i = 0; i < depth; i++)
					sb.append("    ");
				recursivToString(s, sb, depth);
			}
		}
	}

	@Override
	public String toString() {
		return "tl : (" + this.x1 + "," + this.y1 + "), br :(" + this.x2 + "," + this.y2 + ")";
	}

	public Layout tighten(Img binary) {
		double[] x = getHistoLimits(binary.projectHorizontally());
		double[] y = getHistoLimits(binary.projectVertically());
		if (x[0] <= x[1] || y[0] <= y[1]) {
			return new Layout(this.getParent(), getX1() + x[0] * (getX2() - getX1()), getX1() + x[1] * (getX2() - getX1()), getY1() + y[0] * (getY2() - getY1()), getY1() + y[1] * (getY2() - getY1()));
		} else {
			return new Layout(this.getParent(), 0, 0, 0, 0);
		}
	}

	public static double[] getHistoLimits(List<Float> hist) {
		int start = 0;
		int end = hist.size() - 1;
		while (start < hist.size() && hist.get(start) >= 255.0)
			start++;
		while (end >= 0 && hist.get(end) >= 255.0)
			end--;
		return new double[] { Integer.valueOf(start).doubleValue() / hist.size(), Integer.valueOf(end + 1).doubleValue() / hist.size() };
	}

	public List<Layout> split(Size morph, Img binary) {
		Double adjustMorph = (Math.log10(binary.width()) - gold) / 100;
		// System.out.println(String.format("width: %dpx; adjust: %.3f", binary.width(), adjustMorph * 100));

		Double morphW = adjustMorph <= 0 ? morph.width : morph.width - adjustMorph;
		Double morphH = morph.height;

		Double verticalParam = Math.floor(morphH * binary.height());
		Double horizontalParam = Math.floor(morphW * binary.width());

		// System.out.printf("width: %dpx; a: %.2f\n", binary.width(), morphW * 100);
		return extractZones(close(verticalParam.intValue(), binary.projectVertically()), close(horizontalParam.intValue(), binary.projectHorizontally()), binary);
	}

	private static boolean[] close(int k, List<Float> histo) {
		boolean[] closed = new boolean[histo.size()];
		Function<Integer, Boolean> isWhite = i -> histo.get(i) >= 255;
		for (int i = 0; i < histo.size() - 1; i++)
			if (!isWhite.apply(i) && isWhite.apply(i + 1)) {
				for (int j = k + 1; j > 0; j--)
					if (i + j < histo.size()) {
						if (!isWhite.apply(i + j)) {
							Arrays.fill(closed, i, i + j + 1, true);
							i += j - 1;
							break;
						}
						closed[i] = !isWhite.apply(i);
					}
			} else
				closed[i] = !isWhite.apply(i);
		if (!closed[histo.size() - 1])
			closed[histo.size() - 1] = !isWhite.apply(histo.size() - 1);
		return closed;
	}

	private List<Layout> extractZones(boolean[] resultV, boolean[] resultH, Img binary) {

		List<double[]> shardsV = getShards(resultV, true);
		List<double[]> shardsH = getShards(resultH, false);
		// System.out.println("Binary size : " + binary.size());
		// System.out.println("Size spit : " + shardsV.size() + " " + shardsH.size());
		List<Layout> shards = new ArrayList<>();
		for (double[] shardv : shardsV)
			for (double[] shardh : shardsH) {
				Layout target = new Layout(this, shardh[0], shardh[1], shardv[0], shardv[1]);
				Img roi = target.getRoi(binary);
				// System.out.println("roi : rows :" + roi.rows() + " , cols :" + roi.cols());
				if (roi.rows() != 0 && roi.cols() != 0)
					shards.add(target.tighten(roi));
			}
		return shards;
	}

	private List<double[]> getShards(boolean[] result, boolean vertical) {
		List<double[]> shards = new ArrayList<>();
		Integer start = result[0] ? 0 : null;
		assert result.length >= 1;
		for (int i = 0; i < result.length - 1; i++)
			if (!result[i] && result[i + 1])
				start = i + 1;
			else if (result[i] && !result[i + 1]) {
				shards.add(vertical ? new double[] { Integer.valueOf(start).doubleValue() / result.length, (Integer.valueOf(i).doubleValue() + 1) / result.length }
						: new double[] { Integer.valueOf(start).doubleValue() / result.length, (Integer.valueOf(i).doubleValue() + 1) / result.length });
				start = null;
			}
		if (result[result.length - 1]) {
			shards.add(vertical ? new double[] { Integer.valueOf(start).doubleValue() / result.length, Integer.valueOf(result.length).doubleValue() / result.length }
					: new double[] { Integer.valueOf(start).doubleValue() / result.length, Integer.valueOf(result.length).doubleValue() / result.length });
			start = null;
		}
		return shards;
	}

	public Layout recursiveSplit(Size morph, int level, Img binary) {
		// System.out.println("level : " + level);
		// System.out.println("Layout : " + this);
		assert binary.size().equals(binary.size());
		if (binary.size().height == 0 || binary.size().width == 0)
			return this;

		if (level <= 0) {
			// Imgproc.rectangle(img.getSrc(), new Point(0, 0), new Point(img.width(), img.height()), new Scalar(255, 0, 0), -1);
			return this;
		}
		List<Layout> shards = split(morph, binary);
		// shards.removeIf(shard -> ((shard.getY2() - shard.getY1()) * binary.size().height) < 2 || ((shard.getX2() - shard.getX1()) * binary.size().width) < 2);
		if (shards.isEmpty()) {
			// Imgproc.rectangle(img.getSrc(), new Point(0, 0), new Point(img.width(), img.height()), new Scalar(0, 0, 255), -1);
			return this;
		}
		if (shards.size() == 1) {
			return this;
		}
		for (Layout shard : shards) {
			shard.recursiveSplit(morph, level - 1, shard.getRoi(binary));
			this.addChild(shard);
		}
		return this;
	}
}
