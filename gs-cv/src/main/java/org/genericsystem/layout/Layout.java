package org.genericsystem.layout;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;

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

	public Rect getLargeRect(Img imgRoot, double deltaW, double deltaH) {
		Rect rect = getRect(imgRoot);
		int adjustW = 3 + Double.valueOf(Math.floor(rect.width * deltaW)).intValue();
		int adjustH = 3 + Double.valueOf(Math.floor(rect.height * deltaH)).intValue();

		Point tl = new Point(rect.tl().x - adjustW > 0 ? rect.tl().x - adjustW : 0, rect.tl().y - adjustH > 0 ? rect.tl().y - adjustH : 0);
		Point br = new Point(rect.br().x + adjustW > imgRoot.width() ? imgRoot.width() : rect.br().x + adjustW, rect.br().y + adjustH > imgRoot.height() ? imgRoot.height() : rect.br().y + adjustH);

		// System.out.println(String.format("tl: %s | rect.tl: %s", tl, rect.tl()));
		// System.out.println(String.format("br: %s | rect.br: %s", br, rect.br()));
		return new Rect(tl, br);
	}

	public double normalizedArea() {
		double result = 0;
		if (this.getChildren().isEmpty()) {
			Point[] pts = getNormalizedTlBr();
			return (pts[1].x - pts[0].x) * (pts[1].y - pts[0].y);
		}
		for (Layout child : getChildren())
			result += child.normalizedArea();
		return result;
	}

	public Point[] getNormalizedTlBr() {
		Point[] parentRect = getParent() != null ? getParent().getNormalizedTlBr() : new Point[] { new Point(0, 0), new Point(1, 1) };
		return new Point[] { new Point(parentRect[0].x + (parentRect[1].x - parentRect[0].x) * getX1(), parentRect[0].y + (parentRect[1].y - parentRect[0].y) * getY1()),
				new Point(parentRect[0].x + (parentRect[1].x - parentRect[0].x) * getX2(), parentRect[0].y + (parentRect[1].y - parentRect[0].y) * getY2()) };
	}

	public double area(Img imgRoot) {
		if (this.getChildren().isEmpty())
			return getRect(imgRoot).area() / (imgRoot.height() * imgRoot.width());
		double result = 0;
		for (Layout child : getChildren())
			result += child.area(imgRoot);

		return result;
	}

	public double computeTotalSurface(Img img) {
		double[] surface = new double[] { 0.0 };
		traverse(getRoi(img), (roi, shard) -> {
			if (shard.getChildren().isEmpty())
				surface[0] += roi.height() * roi.width();
		});
		return surface[0] / (img.width() * img.height());
	}

	// public Layout traverse(Size parentSize, BiConsumer<Size, Layout> visitor) {
	// for (Layout shard : getChildren())
	// shard.traverse(new Size(parentSize.width * (getX2() - getX1()), parentSize.height * (getY2() - getY1())), visitor);
	// visitor.accept(parentSize, this);
	// return this;
	// }

	public Layout traverse(Img img, BiConsumer<Img, Layout> visitor) {
		for (Layout shard : getChildren())
			shard.traverse(shard.getRoi(img), visitor);
		visitor.accept(img, this);
		return this;
	}

	public void draw(Img img, Scalar branchColor, Scalar leafColor, int branchThickness, int leafThickness) {
		traverse(getRoi(img),
				(roi, shard) -> Imgproc.rectangle(roi.getSrc(), new Point(0, 0), new Point(roi.width() - 1, roi.height() - 1), shard.getChildren().isEmpty() ? leafColor : branchColor, shard.getChildren().isEmpty() ? leafThickness : branchThickness));
	}

	public void drawOcrPerspectiveInverse(Img rootImg, Mat homography, Scalar color, int thickness) {
		traverse(getRoi(rootImg), (roi, layout) -> {
			if (layout.getChildren().isEmpty()) {
				MatOfPoint2f results = new MatOfPoint2f();
				Rect rect = layout.getRect(rootImg);
				Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(rect.x + rect.width / 2, rect.y + rect.height / 2))), results, homography);
				Point[] targets = results.toArray();
				Imgproc.line(rootImg.getSrc(), targets[0], new Point(targets[0].x, targets[0].y - 30), color, thickness);
				Imgproc.putText(rootImg.getSrc(), Normalizer.normalize(layout.getBestLabel(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), new Point(targets[0].x - 10, targets[0].y - 30), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
			}
		});
	}

	public void ocrTree(Img rootImg, double deltaW, double deltaH) {
		traverse(rootImg, (root, layout) -> {
			if (layout.getChildren().isEmpty()) {
				if (layout.needOcr()) {
					String ocr = Ocr.doWork(new Mat(rootImg.getSrc(), layout.getLargeRect(rootImg, deltaW, deltaH)));
					if (!"".equals(ocr)) {
						Integer count = layout.getLabels().get(ocr);
						layout.getLabels().put(ocr, 1 + (count != null ? count : 0));
					}
				}
			}
		});
	}

	public void drawOcr(Img rootImg) {
		traverse(rootImg, (root, layout) -> {
			if (layout.getChildren().isEmpty())
				Imgproc.putText(rootImg.getSrc(), Normalizer.normalize(layout.getBestLabel(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), layout.getRect(rootImg).tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
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

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public boolean needOcr() {
		int all = getLabels().values().stream().reduce(0, (i, j) -> i + j);
		for (Entry<String, Integer> entry : getLabels().entrySet()) {
			if (entry.getValue() > all / 3)
				return false;
		}
		return true;
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

	public Layout recursiveSplit(Size constantClose, Size linearClose, int level, Img binary) {
		// if (binary.size().height == 0 || binary.size().width == 0)
		// return this;
		if (level <= 0)
			return this;
		List<Layout> shards = split(constantClose, linearClose, binary);
		shards.removeIf(shard -> ((shard.getY2() - shard.getY1()) * binary.size().height) < 2 || ((shard.getX2() - shard.getX1()) * binary.size().width) < 2);
		if (shards.isEmpty())
			return null;
		for (Layout shard : shards) {
			Layout shardLayout = shard.recursiveSplit(constantClose, linearClose, level - 1, shard.getRoi(binary));
			if (shardLayout != null)
				if (shard.getX1() != 0 || shard.getX2() != 1 || shard.getY1() != 0 || shard.getY2() != 1)
					this.addChild(shard);
		}
		return this;
	}

	public List<Layout> split(Size constantClose, Size linearClose, Img binary) {
		int verticalClose = (int) Math.floor(constantClose.height + linearClose.height * binary.height());
		int horizontalClose = (int) Math.floor(constantClose.width + linearClose.width * binary.width());
		return extractZones(Img.close(binary.projectVertically(), verticalClose), Img.close(binary.projectHorizontally(), horizontalClose), binary);
	}

	private List<Layout> extractZones(boolean[] resultV, boolean[] resultH, Img binary) {
		List<double[]> shardsV = getShards(resultV);
		List<double[]> shardsH = getShards(resultH);
		List<Layout> shards = new ArrayList<>();
		for (double[] shardv : shardsV)
			for (double[] shardh : shardsH) {
				Layout target = new Layout(this, shardh[0], shardh[1], shardv[0], shardv[1]);
				if (shardh[0] != shardh[1] && shardv[0] != shardv[1])
					shards.add(target);
			}
		return shards;
	}

	private List<double[]> getShards(boolean[] result) {
		List<double[]> shards = new ArrayList<>();
		Double start = result[0] ? 0d : null;
		assert result.length >= 1;
		for (int i = 0; i < result.length - 1; i++)
			if (!result[i] && result[i + 1])
				start = (double) (i + 1);
			else if (result[i] && !result[i + 1]) {
				shards.add(new double[] { start / result.length, ((double) i + 1) / result.length });
				start = null;
			}
		if (result[result.length - 1]) {
			shards.add(new double[] { start / result.length, 1 });
			start = null;
		}
		return shards;
	}

}
