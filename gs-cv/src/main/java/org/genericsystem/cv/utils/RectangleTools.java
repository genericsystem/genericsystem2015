package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.opencv.core.Point;
import org.opencv.core.Rect;

/**
 * Static methods to compare {@link Rect} objects.
 * 
 * @author Pierrik Lassalas
 */
public class RectangleTools {

	public static void main(String[] args) {
		Rect rect1 = new Rect(0, 0, 3, 3);
		Rect rect2 = new Rect(1, 3, 3, 1);
		System.out.println("Union: " + getUnion(rect1, rect2));
		System.out.println("Intersection: " + getIntersection(rect1, rect2));
	}

	/**
	 * Method to remove the overlapping {@link Rect}, relying on "non-maximum suppression" to ignore redundant, overlapping boxes.
	 * 
	 * @param boxes - the list of rectangles that need to be filtered
	 * @param overlapThreshold - the overlapping threshold. If the common area between two rectangles is above this threshold, they will be considered as overlapping.
	 * @return a List of non-overlapping {@link Rect}, or an empty List if none was found
	 */
	public static List<Rect> nonMaximumSuppression(List<Rect> boxes, double overlapThreshold) {
		if (boxes == null || boxes.size() == 0)
			return Collections.emptyList();

		// Initialize a list of picked indexes
		List<Integer> pick = new ArrayList<>();

		// Get the coordinates of the boxes tl(x1, y1) and br(x2, y2)
		List<Double> x1 = boxes.stream().map(rect -> rect.tl().x).collect(Collectors.toList());
		List<Double> y1 = boxes.stream().map(rect -> rect.tl().y).collect(Collectors.toList());
		List<Double> x2 = boxes.stream().map(rect -> rect.br().x).collect(Collectors.toList());
		List<Double> y2 = boxes.stream().map(rect -> rect.br().y).collect(Collectors.toList());

		// Compute the areas
		List<Double> area = boxes.stream().map(rect -> rect.area()).collect(Collectors.toList());

		// Get the indexes of the boxes sorted by the br() y coordinates (ascending)
		List<Integer> indx = IntStream.range(0, y2.size()).boxed().sorted((i, j) -> Double.compare(y2.get(i), y2.get(j))).collect(Collectors.toList());

		// Keep looping while some indexes remain in the indx list
		long count = 0L; // TODO: fix an infinite loop problem
		while (indx.size() > 0 && count++ < 10 * boxes.size()) {
			// Grab the last index and add the value to the list of picked indexes
			int last = indx.size() - 1;
			int i = indx.get(last);
			pick.add(i);

			// Find the largest tl(xx1, yy1) coordinates for the start of the box, and the smallest br(xx2, yy2) coordinates for the end of the box
			List<Double> xx1 = IntStream.range(0, x1.size()).filter(idx -> indx.contains(idx)).mapToObj(x1::get).map(x -> Math.max(x1.get(i), x)).collect(Collectors.toList());
			List<Double> yy1 = IntStream.range(0, y1.size()).filter(idx -> indx.contains(idx)).mapToObj(y1::get).map(y -> Math.max(y1.get(i), y)).collect(Collectors.toList());
			List<Double> xx2 = IntStream.range(0, x2.size()).filter(idx -> indx.contains(idx)).mapToObj(x2::get).map(x -> Math.min(x2.get(i), x)).collect(Collectors.toList());
			List<Double> yy2 = IntStream.range(0, y2.size()).filter(idx -> indx.contains(idx)).mapToObj(y2::get).map(y -> Math.min(y2.get(i), y)).collect(Collectors.toList());

			// Compute the width, height and area of the boxes
			List<Double> width = new ArrayList<>();
			List<Double> height = new ArrayList<>();
			List<Double> overlap = new ArrayList<>();
			List<Double> filteredArea = IntStream.range(0, area.size()).filter(idx -> indx.contains(idx)).mapToObj(area::get).collect(Collectors.toList());
			for (int j = 0; j < xx1.size(); ++j) {
				width.add(Math.max(0, xx2.get(j) - xx1.get(j) + 1));
				height.add(Math.max(0, yy2.get(j) - yy1.get(j) + 1));
				overlap.add((width.get(j) * height.get(j)) / filteredArea.get(j));
			}
			// Remove all indexes from the index list whose overlap is above the threshold
			IntStream.range(0, overlap.size()).filter(idx -> overlap.get(idx) > overlapThreshold).boxed().forEach(idx -> indx.remove(idx));
			// XXX this last part can cause an infinite loop when the remove() function fails => not caught in the unit tests!
		}
		List<Rect> res = IntStream.range(0, boxes.size()).filter(idx -> pick.contains(idx)).mapToObj(boxes::get).collect(Collectors.toList());
		return res;
	}

	/**
	 * Compute the common area between two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an array of double. The first value is the percentage of <code>rect1</code> occupied by <code>rect2</code>, and the second value is the percentage of <code>rect2</code> occupied by <code>rect1</code>.
	 */
	public static double[] commonArea(Rect rect1, Rect rect2) {
		double[] result = new double[2];
		Optional<Rect> intersection = getIntersection(rect1, rect2);
		if (intersection.isPresent()) {
			Rect intersect = intersection.get();
			result[0] = intersect.area() / rect1.area();
			result[1] = intersect.area() / rect2.area();
		} else {
			result[0] = 0;
			result[1] = 0;
		}
		return result;
	}

	/**
	 * Compute the inclusive area between two rectangles (intersection area / union area)
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return the percentage of overlap between the two rectangles, defined by <code>intersection.area() / union.area()</code>
	 */
	public static double inclusiveArea(Rect rect1, Rect rect2) {
		Optional<Rect> optional = getIntersection(rect1, rect2);
		if (!optional.isPresent())
			return 0;
		Rect intersection = optional.get();
		Rect union = getUnion(rect1, rect2);
		double area = intersection.area() / union.area();
		return area;
	}

	/**
	 * Compute the intersection of two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an {@link Optional} with the intersecting {@link Rect}, or an empty Optional if no intersection was found
	 */
	public static Optional<Rect> getIntersection(Rect rect1, Rect rect2) {
		// First, check whether a rectangle is contained in the other
		Optional<Rect> insider = getInsider(rect1, rect2);
		if (insider.isPresent())
			return insider;

		// If not, compute the intersection
		double tlX = Math.max(rect1.tl().x, rect2.tl().x);
		double tlY = Math.max(rect1.tl().y, rect2.tl().y);
		double brX = Math.min(rect1.br().x, rect2.br().x);
		double brY = Math.min(rect1.br().y, rect2.br().y);

		if (brX - tlX <= 0 || brY - tlY <= 0) // XXX: swap tl and br if < 0?
			return Optional.empty();
		else
			return Optional.of(new Rect(new Point(tlX, tlY), new Point(brX, brY)));
	}

	/**
	 * Compute the union of two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return the union {@link Rect}
	 */
	public static Rect getUnion(Rect rect1, Rect rect2) {
		// First, check whether a rectangle is contained in the other
		Optional<Rect> inside = getInsider(rect1, rect2);
		if (inside.isPresent()) {
			Rect insider = inside.get();
			return insider.equals(rect1) ? rect2 : rect1;
		}

		// If not, compute the union
		double tlX = Math.min(rect1.tl().x, rect2.tl().x);
		double tlY = Math.min(rect1.tl().y, rect2.tl().y);
		double brX = Math.max(rect1.br().x, rect2.br().x);
		double brY = Math.max(rect1.br().y, rect2.br().y);

		return new Rect(new Point(tlX, tlY), new Point(brX, brY));
	}

	/**
	 * Check whether two rectangles are overlapping. This method is inclusive, e.g. it will return true if the rectangles have only a side or an angle in common.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return true is the rectangles overlap, false otherwise
	 * @throws IllegalArgumentException if at least one of the rectangles is <code>null</code>
	 */
	public static boolean isOverlapping(Rect rect1, Rect rect2) throws IllegalArgumentException {
		if (rect1 == null || rect2 == null)
			throw new IllegalArgumentException("One of the rectangles is null");
		return getIntersection(rect1, rect2).map(rect -> rect.area() > 0 ? true : false).orElse(false);
	}

	/**
	 * Compare 2 rectangles, and returns the smaller rectangle if it is inside the other. Returns an empty {@link Optional} if no rectangles is contained in the other.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an {@link Optional} with the rectangle contained in the other, an empty Optional if no rectangles is contained in the other.
	 */
	public static Optional<Rect> getInsider(Rect rect1, Rect rect2) {
		Point[] points1 = decomposeClockwise(rect1);
		Point[] points2 = decomposeClockwise(rect2);
		boolean isRect2InRect1 = true;
		boolean isRect1InRect2 = true;

		for (Point p : points2) {
			if (!contains(rect1, p))
				isRect2InRect1 = false;
		}

		if (!isRect2InRect1) {
			for (Point p : points1) {
				if (!contains(rect2, p))
					isRect1InRect2 = false;
			}
			if (isRect1InRect2)
				return Optional.of(rect1);
			else
				return Optional.empty();
		} else {
			return Optional.of(rect2);
		}
	}

	/**
	 * Check if a {@link Point} is contained in a {@link Rect} (being inclusive).
	 * 
	 * @param rect - the rectangle
	 * @param p - the point
	 * @return true if <code>p</code> is contained in <code>rect</code>, false otherwise
	 */
	public static boolean contains(Rect rect, Point p) {
		boolean res = rect.tl().x <= p.x && p.x <= rect.br().x && rect.tl().y <= p.y && p.y <= rect.br().y;
		return res;
	}

	/**
	 * Decompose a {@link Rect} in four points starting from tl(), clockwise.
	 * 
	 * @param rect - the rectangle
	 * @return an array of {@link Point} in the order tl, tr, br, bl
	 */
	public static Point[] decomposeClockwise(Rect rect) {
		Point[] points = new Point[] { rect.tl(), new Point(rect.br().x, rect.tl().y), rect.br(), new Point(rect.tl().x, rect.br().y) };
		return points;
	}
}
