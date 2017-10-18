package org.genericsystem.cv.utils;

import java.util.Optional;

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
	 * Compare 2 rectangles, and returns the smaller rectangle if it is inside the other. Returns an empty {@link Optional} if no rectangles is contained in the other.
	 * 
	 * @param rect1 - the first rectangle
	 * @param rect2 - the second rectangle
	 * @return an {@link Optional} with the rectangle contained in the other, an empty Optional if no rectangles is contained in the other.
	 */
	private static Optional<Rect> getInsider(Rect rect1, Rect rect2) {
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
	private static boolean contains(Rect rect, Point p) {
		boolean res = rect.tl().x <= p.x && p.x <= rect.br().x && rect.tl().y <= p.y && p.y <= rect.br().y;
		return res;
	}

	/**
	 * Decompose a {@link Rect} in four points starting from tl(), clockwise.
	 * 
	 * @param rect - the rectangle
	 * @return an array of {@link Point} in the order tl, tr, br, bl
	 */
	private static Point[] decomposeClockwise(Rect rect) {
		Point[] points = new Point[] { rect.tl(), new Point(rect.br().x, rect.tl().y), rect.br(), new Point(rect.tl().x, rect.br().y) };
		return points;
	}
}
