package org.genericsystem.cv.application.mesh;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Point;

public abstract class Points {
	private HashMap<Key, IndexedPoint> internal = new HashMap<>();
	private List<IndexedPoint> pointIndex = new ArrayList<>();

	protected final int xBorder;
	protected final int yBorder;

	public Points(int xBorder, int yBorder) {
		this.xBorder = xBorder;
		this.yBorder = yBorder;

	}

	private Point mock(Point p1, Point p2, Point p3) {
		return new Point(p3.x + p2.x - p1.x, p3.y + p2.y - p1.y);
	}

	private Point extrapole(Point p1, Point p2) {
		return new Point(p2.x + p2.x - p1.x, p2.y + p2.y - p1.y);
	}

	private Point intersectExtra(Point p1, Point p2, Point p3, Point p4) {
		double x1 = p1.x;
		double x2 = p2.x;
		double x3 = p3.x;
		double x4 = p4.x;
		double y1 = p1.y;
		double y2 = p2.y;
		double y3 = p3.y;
		double y4 = p4.y;

		double x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));
		double y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));
		return new Point(x, y);

	}

	static double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
	}

	static double[] cross2D(double[] a, double b[]) {
		return uncalibrate(cross(a, b));
	}

	static double[] uncalibrate(double[] a) {
		return new double[] { a[0] / a[2], a[1] / a[2], 1 };
	}

	protected IndexedPoint createIndexedPoint(Point point) {
		IndexedPoint indexedPoint = new IndexedPoint(pointIndex.size(), point);
		pointIndex.add(indexedPoint);
		return indexedPoint;
	}

	// points = new Point[2 * halfHeight + 1][2 * halfWidth + 1];
	public void put(int i, int j, IndexedPoint point) {
		// System.out.println(i + " " + j);
		internal.put(new Key(i, j), point);
	}

	public IndexedPoint get(int i, int j) {
		return internal.get(new Key(i, j));
		// return internal.get(new Key(i + halfHeight, j + halfWidth));
	}

	public Point getPoint(int i, int j) {
		assert internal.get(new Key(i, j)) != null : "None : " + i + " " + j;
		return internal.get(new Key(i, j)).getPoint();
	}

	public List<Point> getPointIndex() {
		return pointIndex.stream().map(IndexedPoint::getPoint).collect(Collectors.toList());
	}

	public class IndexedPoint {
		private final int index;
		private final Point point;

		private IndexedPoint(int index, Point point) {
			this.index = index;
			this.point = point;
		}

		public int getIndex() {
			return index;
		}

		public Point getPoint() {
			return point;
		}
	}
}