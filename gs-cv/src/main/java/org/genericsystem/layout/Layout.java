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
import org.genericsystem.cv.utils.OCRPlasty;
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

	public Rect getLargeRect(Img imgRoot, double deltaW, double deltaH) {
		Rect rect = getRect(imgRoot);

		// System.out.println(String.format("rect.width: %d; rect.height: %d", rect.width, rect.height));

		int adjustW = 3 + Double.valueOf(Math.floor(rect.width * deltaW)).intValue();
		int adjustH = 3 + Double.valueOf(Math.floor(rect.height * deltaH)).intValue();

		// System.out.println(String.format("adjustW: %d; adjustH: %d", adjustW, adjustH));

		Point tl = new Point(rect.tl().x - adjustW > 0 ? rect.tl().x - adjustW : 0, rect.tl().y - adjustH > 0 ? rect.tl().y - adjustH : 0);
		Point br = new Point(rect.br().x + adjustW > imgRoot.width() ? imgRoot.width() : rect.br().x + adjustW, rect.br().y + adjustH > imgRoot.height() ? imgRoot.height() : rect.br().y + adjustH);

		// System.out.println(String.format("tl: %s | rect.tl: %s", tl, rect.tl()));
		// System.out.println(String.format("br: %s | rect.br: %s", br, rect.br()));
		return new Rect(tl, br);
	}

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
						// int all = layout.getLabels().values().stream().reduce(0, (i, j) -> i + j);
						// layout.getLabels().entrySet().forEach(entry -> {
						// if (entry.getValue() > all / 10)
						// Imgproc.putText(rootImg.getSrc(), Normalizer.normalize(entry.getKey(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), layout.getRect(rootImg).tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
						// });
						// Imgproc.putText(rootImg.getSrc(), layout.getBestLabel(), layout.getRect(rootImg).tl(), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
						// System.out.println(layout.getBestLabel());
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

	public boolean nodeIsEqual(Layout otherLayout, double accuracy) {
		double delta = Math.abs((otherLayout.getX1() - this.x1) / (this.x2 - this.x1));
		if (delta < accuracy) {
			delta = Math.abs((otherLayout.getX2() - this.x2) / (this.x2 - this.x1));
			if (delta < accuracy) {
				delta = Math.abs((otherLayout.getY1() - this.y1) / (this.y2 - this.y1));
				if (delta < accuracy) {
					delta = Math.abs((otherLayout.getY2() - this.y2) / (this.y2 - this.y1));
					if (delta < accuracy)
						return true;
				}
			}
		}
		return false;
	}

	public boolean belongsToRoot(Layout otherLayout, double accuracy) {
		// returns true if this is contained in otherLayout, fixed root
		List<Layout> counterparts = new ArrayList<>();
		if (this.hasChildren()) {
			for (Layout child : this.children) {
				boolean hasCounterpart = false;
				for (Layout node : otherLayout.getChildren()) {
					if (!counterparts.contains(node) && child.nodeIsEqual(node, accuracy)) {
						counterparts.add(node);
						if (!child.belongsToRoot(node, accuracy))
							return false;
						hasCounterpart = true;
						break;
					}
				}
				if (!hasCounterpart)
					return false;
			}
		}
		return true;
	}

	public List<Layout> belongsToDesc(Layout otherLayout, double accuracy, List<Layout> previous) {
		// returns the list of descendants of otherLayout (including itself) which contain this
		List<Layout> containingDescendants = previous;
		if (this.belongsToRoot(otherLayout, accuracy))
			containingDescendants.add(otherLayout);
		for (Layout child : otherLayout.getChildren())
			containingDescendants = belongsToDesc(child, accuracy, containingDescendants);
		return containingDescendants;
	}

	private String getConsolidatedLabel() {
		String undefined = "?";
		return getLabels().isEmpty() ? undefined : OCRPlasty.correctStrings(new ArrayList<>(getLabels().keySet()), OCRPlasty.RANSAC.NORM_LEVENSHTEIN).orElse(undefined);
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

	// public Layout tighten(Img binary) {
	// double[] x = getHistoLimits(binary.projectHorizontally());
	// double[] y = getHistoLimits(binary.projectVertically());
	// if (x[0] <= x[1] || y[0] <= y[1]) {
	// return new Layout(this.getParent(), getX1() + x[0] * (getX2() - getX1()), getX1() + x[1] * (getX2() - getX1()), getY1() + y[0] * (getY2() - getY1()), getY1() + y[1] * (getY2() - getY1()));
	// } else {
	// return new Layout(this.getParent(), 0, 0, 0, 0);
	// }
	// }

	public static double[] getHistoLimits(List<Boolean> hist) {
		int start = 0;
		int end = hist.size() - 1;
		while (start < hist.size() && !hist.get(start))
			start++;
		while (end >= 0 && !hist.get(end))
			end--;
		return new double[] { Integer.valueOf(start).doubleValue() / hist.size(), Integer.valueOf(end + 1).doubleValue() / hist.size() };
	}

	public List<Layout> split(Size morph, Img binary) {
		// TODO: refactor parameters to array?
		// Morphology adjustment upon Img size: c / (b + d * x)
		// width
		double cw = Double.valueOf(2);
		double bw = Double.valueOf(0);
		double dw = Double.valueOf(1);
		double adjustMorphW = cw / (bw + dw * binary.width());
		// height
		double ch = Double.valueOf(0.4);
		double bh = Double.valueOf(0);
		double dh = Double.valueOf(1);
		double adjustMorphH = ch / (bh + dh * binary.height());

		// Adjust the morphology for both height and width
		double morphW = morph.width + adjustMorphW;
		double morphH = morph.height + adjustMorphH;

		int verticalParam = Double.valueOf(Math.floor(morphH * binary.height())).intValue();
		int horizontalParam = Double.valueOf(Math.floor(morphW * binary.width())).intValue();
		return extractZones(Img.close(binary.projectVertically(), verticalParam), Img.close(binary.projectHorizontally(), horizontalParam), binary);
	}

	private List<Layout> extractZones(boolean[] resultV, boolean[] resultH, Img binary) {

		List<double[]> shardsV = getShards(resultV, true);
		List<double[]> shardsH = getShards(resultH, false);
		List<Layout> shards = new ArrayList<>();
		for (double[] shardv : shardsV)
			for (double[] shardh : shardsH) {
				Layout target = new Layout(this, shardh[0], shardh[1], shardv[0], shardv[1]);
				// Img roi = target.getRoi(binary);
				// System.out.println("roi : rows :" + roi.rows() + " , cols :" + roi.cols());
				if (shardh[0] != shardh[1] && shardv[0] != shardv[1])
					shards.add(target);
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
		// System.out.println("AAA" + binary.size());
		if (binary.size().height == 0 || binary.size().width == 0)
			return this;
		if (level <= 0) {
			// Imgproc.rectangle(img.getSrc(), new Point(0, 0), new Point(img.width(), img.height()), new Scalar(255, 0,
			// 0), -1);
			return this;
		}
		// System.out.println("BBB");

		List<Layout> shards = split(morph, binary);
		// System.out.println("CCC" + shards.size());

		shards.removeIf(shard -> ((shard.getY2() - shard.getY1()) * binary.size().height) < 2 || ((shard.getX2() - shard.getX1()) * binary.size().width) < 2);
		// System.out.println("DDD" + shards.size());

		if (shards.isEmpty()) {
			// Imgproc.rectangle(img.getSrc(), new Point(0, 0), new Point(img.width(), img.height()), new Scalar(0, 0,
			// 255), -1);
			return this;
		}
		// if (shards.size() == 1) {
		// return this;
		// }
		for (Layout shard : shards) {
			shard.recursiveSplit(morph, level - 1, shard.getRoi(binary));
			this.addChild(shard);
		}
		return this;
	}
}
