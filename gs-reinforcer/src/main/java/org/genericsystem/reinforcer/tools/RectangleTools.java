package org.genericsystem.reinforcer.tools;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Static methods to compare {@link GSRect} objects.
 * 
 * @author Pierrik Lassalas
 */
public class RectangleTools {

	public static final double DEFAULT_EPSILON = 0.2;
	public static final double DEFAULT_GROUP_THRESHOLD = 0;

	public static void main(String[] args) {
		GSRect rect1 = new GSRect(0, 0, 3, 3);
		GSRect rect2 = new GSRect(1, 3, 3, 1);
		System.out.println("Union: " + getUnion(rect1, rect2));
		System.out.println("Intersection: " + getIntersection(rect1, rect2));

		GSRect r1 = new GSRect(0, 0, 100, 200);
		GSRect r2 = new GSRect(25, 25, 100, 200);
		System.out.println(getDelta(r1, r2, 0.2));

		GSRect r3 = new GSRect(0, 0, 3, 4);

		System.out.println(groupRectangles(Arrays.asList(rect1, rect2, r1, r2, r3), MERGE_METHOD.MEAN));
	}

	/**
	 * Describe the method used to merge multiple rectangles.
	 * 
	 * @author Pierrik Lassalas
	 */
	public enum MERGE_METHOD {
		UNION,
		INTERSECTION,
		MEAN;
	}

	/**
	 * Group similar rectangles in a list in clusters, then attempt to remove the overlaps. This overloaded method use the default threshold values.<br>
	 * See {@link #groupRectangles(List, double, double, MERGE_METHOD)}
	 * 
	 * @param input - the initial list of rectangles
	 * @param method - the method used to merge the elements of a cluster into a single rectangle
	 * @return - a simplified list of rectangles
	 */
	public static List<GSRect> groupRectangles(List<GSRect> input, MERGE_METHOD method) {
		return groupRectangles(input, DEFAULT_EPSILON, DEFAULT_GROUP_THRESHOLD, method);
	}

	/**
	 * Group similar rectangles in a list in clusters, then attempt to remove the overlaps.
	 * 
	 * @param input - the initial list of rectangles
	 * @param eps - a coefficient (> 0) used to determine if two rectangles overlap
	 * @param groupThreshold - a threshold used to eliminate small clusters of rectangles. In other words, a cluster is removed if its size is less than the threshold.
	 * @param method - the method used to merge the elements of a cluster into a single rectangle
	 * @return - a simplified list of rectangles
	 */
	public static List<GSRect> groupRectangles(List<GSRect> input, double eps, double groupThreshold, MERGE_METHOD method) {
		List<List<GSRect>> filtered = cluster(input, eps).stream().filter(sublist -> sublist.size() > groupThreshold).collect(Collectors.toList());
		Map<GSRect, Integer> map = new ConcurrentHashMap<>();

		final Function<List<GSRect>, GSRect> merge;
		switch (method) {
		case UNION:
			merge = list -> list.size() <= 1 ? list.get(0) : list.stream().reduce(list.get(0), (r, total) -> getUnion(r, total));
			break;
		case INTERSECTION:
			merge = list -> list.size() <= 1 ? list.get(0) : list.stream().reduce(list.get(0), (r, total) -> getIntersection(r, total).orElse(total));
			break;
		default:
		case MEAN:
			merge = list -> list.size() <= 1 ? list.get(0) : getMean(list);
			break;
		}
		filtered.forEach(clustered -> map.put(merge.apply(clustered), clustered.size()));

		Iterator<Entry<GSRect, Integer>> outerIt = map.entrySet().iterator();
		while (outerIt.hasNext()) {
			Entry<GSRect, Integer> outer = outerIt.next();

			Iterator<Entry<GSRect, Integer>> innerIt = map.entrySet().iterator();
			while (innerIt.hasNext()) {
				Entry<GSRect, Integer> entry = innerIt.next();
				if (!outer.equals(entry)) {
					Optional<GSRect> match = group(outer.getKey(), outer.getValue(), entry.getKey(), entry.getValue(), eps);
					if (match.isPresent()) {
						GSRect rect = match.get();
						if (entry.getKey().equals(rect)) {
							innerIt.remove();
						} else {// <=> rect.equals(first.getKey())
							outerIt.remove();
							break;
						}
					}
				}
			}
		}

		return map.entrySet().stream().map(entry -> entry.getKey()).collect(Collectors.toList());
	}

	/**
	 * Group the elements of a list of {@link GSRect} in clusters, depending on their similarity.
	 * 
	 * @param input - the input list of rectangles
	 * @param eps - a coefficient (> 0) used to determine if two rectangles overlap
	 * @return a list of list of rectangles, grouped in clusters
	 */
	public static List<List<GSRect>> cluster(List<GSRect> input, double eps) {
		List<GSRect> copy = new ArrayList<>(input);
		List<List<GSRect>> output = new ArrayList<>();
		while (copy.size() > 0) {
			GSRect first = copy.get(0);
			List<GSRect> clustered = new ArrayList<>();
			clustered.add(first);
			for (GSRect r : copy) {
				if (!r.equals(first) && isInCluster(first, r, eps))
					clustered.add(r);
			}
			output.add(clustered);
			copy.removeAll(clustered);
		}
		return output;
	}

	/**
	 * Check whether two rectangles can be considered as part of the same cluster.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @param eps - a coefficient used to determine the maximum tolerable delta between the two rectangles
	 * @return true if the rectangles can be considered as part of the same cluster, false otherwise
	 */
	public static boolean isInCluster(GSRect rect1, GSRect rect2, double eps) {
		double delta = getDelta(rect1, rect2, eps);
		return Math.abs(rect1.tl().getX() - rect2.tl().getX()) <= delta && Math.abs(rect1.tl().getY() - rect2.tl().getY()) <= delta && Math.abs(rect1.br().getX() - rect2.br().getX()) <= delta && Math.abs(rect1.br().getY() - rect2.br().getY()) <= delta;
	}

	private static double getDelta(GSRect rect1, GSRect rect2, double eps) {
		return eps * (Math.min(rect1.getWidth(), rect2.getWidth()) + Math.min(rect1.getHeight(), rect2.getHeight())) / 2;
	}

	private static Optional<GSRect> group(GSRect rect1, int count1, GSRect rect2, int count2, double eps) {
		final GSRect bigger, smaller;
		final int biggerCount, smallerCount;
		if (rect1.area() > rect2.area()) {
			bigger = rect1;
			biggerCount = count1;
			smaller = rect2;
			smallerCount = count2;
		} else {
			bigger = rect2;
			biggerCount = count2;
			smaller = rect1;
			smallerCount = count1;
		}
		double dx = eps * bigger.getWidth();
		double dy = eps * bigger.getHeight();

		boolean res = smaller.tl().getX() >= bigger.tl().getX() - dx;
		res = res && smaller.tl().getY() >= bigger.tl().getY() - dy;
		res = res && smaller.br().getX() <= bigger.br().getX() + dx;
		res = res && smaller.br().getY() <= bigger.br().getY() + dy;
		res = res && (biggerCount > Math.max(3, smallerCount) || smallerCount < 3);

		if (res) {
			// Smaller can be removed. Return smaller
			return Optional.of(smaller);
		} else {
			// Smaller can't be removed. Return Optional.empty?
			return Optional.empty();
		}
	}

	/**
	 * Compute the common area between two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an array of double. The first value is the percentage of <code>rect1</code> occupied by <code>rect2</code>, and the second value is the percentage of <code>rect2</code> occupied by <code>rect1</code>.
	 */
	public static double[] commonArea(GSRect rect1, GSRect rect2) {
		double[] result = new double[2];
		Optional<GSRect> intersection = getIntersection(rect1, rect2);
		if (intersection.isPresent()) {
			GSRect intersect = intersection.get();
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
	public static double inclusiveArea(GSRect rect1, GSRect rect2) {
		Optional<GSRect> optional = getIntersection(rect1, rect2);
		if (!optional.isPresent())
			return 0;
		GSRect intersection = optional.get();
		GSRect union = getUnion(rect1, rect2);
		double area = intersection.area() / union.area();
		return area;
	}

	/**
	 * Compute the intersection of two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an {@link Optional} with the intersecting {@link GSRect}, or an empty Optional if no intersection was found
	 */
	public static Optional<GSRect> getIntersection(GSRect rect1, GSRect rect2) {
		// First, check whether a rectangle is contained in the other
		Optional<GSRect> insider = getInsider(rect1, rect2);
		if (insider.isPresent())
			return insider;

		// If not, compute the intersection
		double tlX = Math.max(rect1.tl().getX(), rect2.tl().getX());
		double tlY = Math.max(rect1.tl().getY(), rect2.tl().getY());
		double brX = Math.min(rect1.br().getX(), rect2.br().getX());
		double brY = Math.min(rect1.br().getY(), rect2.br().getY());

		if (brX - tlX <= 0 || brY - tlY <= 0) // XXX: swap tl and br if < 0?
			return Optional.empty();
		else
			return Optional.of(new GSRect(new GSPoint(tlX, tlY), new GSPoint(brX, brY)));
	}

	/**
	 * Compute the union of two rectangles.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return the union {@link GSRect}
	 */
	public static GSRect getUnion(GSRect rect1, GSRect rect2) {
		// First, check whether a rectangle is contained in the other
		Optional<GSRect> inside = getInsider(rect1, rect2);
		if (inside.isPresent()) {
			GSRect insider = inside.get();
			return insider.equals(rect1) ? rect2 : rect1;
		}

		// If not, compute the union
		double tlX = Math.min(rect1.tl().getX(), rect2.tl().getX());
		double tlY = Math.min(rect1.tl().getY(), rect2.tl().getY());
		double brX = Math.max(rect1.br().getX(), rect2.br().getX());
		double brY = Math.max(rect1.br().getY(), rect2.br().getY());

		return new GSRect(new GSPoint(tlX, tlY), new GSPoint(brX, brY));
	}

	/**
	 * Compute a mean rectangle from a list of rectangles.
	 * 
	 * @param rects - a list of rectangles
	 * @return the mean {@link GSRect}
	 */
	public static GSRect getMean(List<GSRect> rects) {
		if (rects == null || rects.isEmpty())
			throw new IllegalArgumentException("Unable to compute mean on a null or empty list");
		if (rects.size() == 1)
			return rects.get(0);
		double tlx = 0, tly = 0, brx = 0, bry = 0;
		for (GSRect r : rects) {
			tlx += r.tl().getX();
			tly += r.tl().getY();
			brx += r.br().getX();
			bry += r.br().getY();
		}
		tlx /= rects.size();
		tly /= rects.size();
		brx /= rects.size();
		bry /= rects.size();
		return new GSRect(new GSPoint(tlx, tly), new GSPoint(brx, bry));
	}

	/**
	 * Check whether two rectangles are overlapping. This method is inclusive, e.g. it will return true if the rectangles have only a side or an angle in common.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return true is the rectangles overlap, false otherwise
	 * @throws IllegalArgumentException if at least one of the rectangles is <code>null</code>
	 */
	public static boolean isOverlapping(GSRect rect1, GSRect rect2) throws IllegalArgumentException {
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
	public static Optional<GSRect> getInsider(GSRect rect1, GSRect rect2) {
		GSPoint[] points1 = decomposeClockwise(rect1);
		GSPoint[] points2 = decomposeClockwise(rect2);
		boolean isGSRect2InGSRect1 = true;
		boolean isGSRect1InGSRect2 = true;

		for (GSPoint p : points2) {
			if (!contains(rect1, p))
				isGSRect2InGSRect1 = false;
		}

		if (!isGSRect2InGSRect1) {
			for (GSPoint p : points1) {
				if (!contains(rect2, p))
					isGSRect1InGSRect2 = false;
			}
			if (isGSRect1InGSRect2)
				return Optional.of(rect1);
			else
				return Optional.empty();
		} else {
			return Optional.of(rect2);
		}
	}

	/**
	 * Check if a {@link GSPoint} is contained in a {@link GSRect} (being inclusive).
	 * 
	 * @param rect - the rectangle
	 * @param p - the point
	 * @return true if <code>p</code> is contained in <code>rect</code>, false otherwise
	 */
	public static boolean contains(GSRect rect, GSPoint p) {
		boolean res = rect.tl().getX() <= p.getX() && p.getX() <= rect.br().getX() && rect.tl().getY() <= p.getY() && p.getY() <= rect.br().getY();
		return res;
	}

	/**
	 * Decompose a {@link GSRect} in four points starting from tl(), clockwise.
	 * 
	 * @param rect - the rectangle
	 * @return an array of {@link GSPoint} in the order tl, tr, br, bl
	 */
	public static GSPoint[] decomposeClockwise(GSRect rect) {
		GSPoint[] points = new GSPoint[] { rect.tl(), new GSPoint(rect.br().getX(), rect.tl().getY()), rect.br(), new GSPoint(rect.tl().getX(), rect.br().getY()) };
		return points;
	}

	/**
	 * Method to remove the overlapping {@link GSRect}, relying on "non-maximum suppression" to ignore redundant, overlapping boxes.
	 * 
	 * @param boxes - the list of rectangles that need to be filtered
	 * @param overlapThreshold - the overlapping threshold. If the common area between two rectangles is above this threshold, they will be considered as overlapping.
	 * @return a List of non-overlapping {@link GSRect}, or an empty List if none was found
	 */
	public static List<GSRect> nonMaximumSuppression(List<GSRect> boxes, double overlapThreshold) {
		if (boxes == null || boxes.size() == 0)
			return Collections.emptyList();

		// Initialize a list of picked indexes
		List<Integer> pick = new ArrayList<>();

		// Get the coordinates of the boxes tl(x1, y1) and br(x2, y2)
		List<Double> x1 = boxes.stream().map(rect -> rect.tl().getX()).collect(Collectors.toList());
		List<Double> y1 = boxes.stream().map(rect -> rect.tl().getY()).collect(Collectors.toList());
		List<Double> x2 = boxes.stream().map(rect -> rect.br().getX()).collect(Collectors.toList());
		List<Double> y2 = boxes.stream().map(rect -> rect.br().getY()).collect(Collectors.toList());

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
		List<GSRect> res = IntStream.range(0, boxes.size()).filter(idx -> pick.contains(idx)).mapToObj(boxes::get).collect(Collectors.toList());
		return res;
	}
}
