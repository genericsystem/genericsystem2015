package org.genericsystem.cv.application.mesh;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.application.Interpolator;
import org.opencv.core.Point;

public class Points {
	private HashMap<Key, IndexedPoint> internal = new HashMap<>();
	private List<IndexedPoint> pointIndex = new ArrayList<>();
	private final Interpolator interpolator;
	private final int xBorder;
	private final int yBorder;
	private final int extraWidth = 1;
	private final int extraHeight = 1;

	public Points(Point imgCenter, int halfWidth, int halfHeight, double deltaX, double deltaY, int xBorder, int yBorder, Interpolator interpolator) {
		this.interpolator = interpolator;
		this.xBorder = xBorder;
		this.yBorder = yBorder;
		put(0, 0, createIndexedPoint(imgCenter));
		// for (int j = 1; j <= halfWidth; j++)
		// put(0, j, createIndexedPoint((j <= (halfWidth - extraWidth)) ? horizontalMove(getPoint(0, j - 1), deltaX) : extrapole(getPoint(0, j - 2), getPoint(0, j - 1))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(0, j, createIndexedPoint((j >= (-halfWidth + extraWidth)) ? horizontalMove(getPoint(0, j + 1), -deltaX) : extrapole(getPoint(0, j + 2), getPoint(0, j + 1))));
		// for (int i = 1; i <= halfHeight; i++) {
		// put(i, 0, createIndexedPoint((i <= (halfHeight - extraHeight)) ? verticalMove(getPoint(i - 1, 0), deltaY) : extrapole(getPoint(i - 2, 0), getPoint(i - 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j,
		// createIndexedPoint((i <= (halfHeight - extraHeight) && (j <= halfWidth - extraWidth)) ? intersect(getPoint(i, j - 1), getPoint(i - 1, j))
		// : intersectExtra(j != 1 ? getPoint(i, j - 2) : mock(getPoint(i - 1, 0), getPoint(i - 1, 1), getPoint(i, 0)), getPoint(i, j - 1), i != 1 ? getPoint(i - 2, j) : mock(getPoint(0, j - 1), getPoint(1, j - 1), getPoint(0, j)),
		// getPoint(i - 1, j))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint((i <= (halfHeight - extraHeight) && j >= (-halfWidth + extraWidth)) ? intersect(getPoint(i, j + 1), getPoint(i - 1, j))
		// : intersectExtra(getPoint(i, j + 2), getPoint(i, j + 1), i != 1 ? getPoint(i - 2, j) : mock(getPoint(0, j + 1), getPoint(1, j + 1), getPoint(0, j)), getPoint(i - 1, j))));
		// }
		// for (int i = -1; i >= -halfHeight; i--) {
		// put(i, 0, createIndexedPoint((i >= (-halfHeight + extraHeight)) ? verticalMove(getPoint(i + 1, 0), -deltaY) : extrapole(getPoint(i + 2, 0), getPoint(i + 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j, createIndexedPoint(((i >= -halfHeight + extraHeight) && j <= (halfWidth - extraWidth)) ? intersect(getPoint(i, j - 1), getPoint(i + 1, j))
		// : intersectExtra(j != 1 ? getPoint(i, j - 2) : mock(getPoint(i + 1, 0), getPoint(i + 1, 1), getPoint(i, 0)), getPoint(i, j - 1), getPoint(i + 2, j), getPoint(i + 1, j))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint(
		// (i >= (-halfHeight + extraHeight) && (j >= -halfWidth + extraWidth)) ? intersect(getPoint(i, j + 1), getPoint(i + 1, j)) : intersectExtra(getPoint(i, j + 2), getPoint(i, j + 1), getPoint(i + 2, j), getPoint(i + 1, j))));
		// }

		// for (int j = 1; j <= halfWidth; j++)
		// put(0, j, createIndexedPoint((j <= (halfWidth - extraWidth)) ? horizontalMove(getPoint(0, j - 1), deltaX) : extrapole(getPoint(0, j - 2), getPoint(0, j - 1))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(0, j, createIndexedPoint((j >= (-halfWidth + extraWidth)) ? horizontalMove(getPoint(0, j + 1), -deltaX) : extrapole(getPoint(0, j + 2), getPoint(0, j + 1))));
		// for (int i = 1; i <= halfHeight; i++) {
		// put(i, 0, createIndexedPoint((i <= (halfHeight - extraHeight)) ? verticalMove(getPoint(i - 1, 0), deltaY) : extrapole(getPoint(i - 2, 0), getPoint(i - 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j, createIndexedPoint((i <= (halfHeight - extraHeight) && (j <= halfWidth - extraWidth)) ? intersect(getPoint(i, j - 1), getPoint(i - 1, j))
		// : intersectExtra(getPoint(i, j - 1), mock(getPoint(i - 1, j - 1), getPoint(i - 1, j), getPoint(i, j - 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j - 1), getPoint(i, j - 1), getPoint(i - 1, j)))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint((i <= (halfHeight - extraHeight) && j >= (-halfWidth + extraWidth)) ? intersect(getPoint(i, j + 1), getPoint(i - 1, j))
		// : intersectExtra(getPoint(i, j + 1), mock(getPoint(i - 1, j + 1), getPoint(i - 1, j), getPoint(i, j + 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j + 1), getPoint(i, j + 1), getPoint(i - 1, j)))));
		// }
		// for (int i = -1; i >= -halfHeight; i--) {
		// put(i, 0, createIndexedPoint((i >= (-halfHeight + extraHeight)) ? verticalMove(getPoint(i + 1, 0), -deltaY) : extrapole(getPoint(i + 2, 0), getPoint(i + 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j, createIndexedPoint(((i >= -halfHeight + extraHeight) && j <= (halfWidth - extraWidth)) ? intersect(getPoint(i, j - 1), getPoint(i + 1, j))
		// : intersectExtra(getPoint(i, j - 1), mock(getPoint(i + 1, j - 1), getPoint(i + 1, j), getPoint(i, j - 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j - 1), getPoint(i, j - 1), getPoint(i + 1, j)))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint((i >= (-halfHeight + extraHeight) && (j >= -halfWidth + extraWidth)) ? intersect(getPoint(i, j + 1), getPoint(i + 1, j))
		// : intersectExtra(getPoint(i, j + 1), mock(getPoint(i + 1, j + 1), getPoint(i + 1, j), getPoint(i, j + 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j + 1), getPoint(i, j + 1), getPoint(i + 1, j)))));
		// }

		for (int j = 1; j <= halfWidth; j++)
			put(0, j, createIndexedPoint(horizontalMove(getPoint(0, j - 1), deltaX)));
		for (int j = -1; j >= -halfWidth; j--)
			put(0, j, createIndexedPoint(horizontalMove(getPoint(0, j + 1), -deltaX)));
		for (int i = 1; i <= halfHeight; i++) {
			put(i, 0, createIndexedPoint(verticalMove(getPoint(i - 1, 0), deltaY)));
			for (int j = 1; j <= halfWidth; j++)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j - 1), getPoint(i - 1, j))));
			for (int j = -1; j >= -halfWidth; j--)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j + 1), getPoint(i - 1, j))));
		}
		for (int i = -1; i >= -halfHeight; i--) {
			put(i, 0, createIndexedPoint(verticalMove(getPoint(i + 1, 0), -deltaY)));
			for (int j = 1; j <= halfWidth; j++)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j - 1), getPoint(i + 1, j))));
			for (int j = -1; j >= -halfWidth; j--)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j + 1), getPoint(i + 1, j))));
		}
		//
		// for (int j = halfWidth + 1; j <= halfWidth + extraWidth; j++)
		// put(0, j, createIndexedPoint(extrapole(getPoint(0, j - 2), getPoint(0, j - 1))));
		// for (int j = -halfWidth - 1; j >= -halfWidth - extraWidth; j--)
		// put(0, j, createIndexedPoint(extrapole(getPoint(0, j + 2), getPoint(0, j + 1))));
		// for (int i = halfHeight + 1; i <= halfHeight + extraHeight; i++) {
		// put(i, 0, createIndexedPoint(extrapole(getPoint(i - 2, 0), getPoint(i - 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j - 1), mock(getPoint(i - 1, j - 1), getPoint(i - 1, j), getPoint(i, j - 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j - 1), getPoint(i, j - 1), getPoint(i - 1, j)))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j + 1), mock(getPoint(i - 1, j + 1), getPoint(i - 1, j), getPoint(i, j + 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j + 1), getPoint(i, j + 1), getPoint(i - 1, j)))));
		// }
		// for (int i = -halfHeight - 1; i >= -halfHeight - extraHeight; i--) {
		// put(i, 0, createIndexedPoint(extrapole(getPoint(i + 2, 0), getPoint(i + 1, 0))));
		// for (int j = 1; j <= halfWidth; j++)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j - 1), mock(getPoint(i + 1, j - 1), getPoint(i + 1, j), getPoint(i, j - 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j - 1), getPoint(i, j - 1), getPoint(i + 1, j)))));
		// for (int j = -1; j >= -halfWidth; j--)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j + 1), mock(getPoint(i + 1, j + 1), getPoint(i + 1, j), getPoint(i, j + 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j + 1), getPoint(i, j + 1), getPoint(i + 1, j)))));
		// }
		// for (int i = 1; i <= halfHeight + extraHeight; i++) {
		// put(i, 0, createIndexedPoint(extrapole(getPoint(i - 2, 0), getPoint(i - 1, 0))));
		// for (int j = halfWidth + 1; j <= halfWidth + extraWidth; j++)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j - 1), mock(getPoint(i - 1, j - 1), getPoint(i - 1, j), getPoint(i, j - 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j - 1), getPoint(i, j - 1), getPoint(i - 1, j)))));
		// for (int j = -halfWidth - 1; j >= -halfWidth - extraWidth; j--)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j + 1), mock(getPoint(i - 1, j + 1), getPoint(i - 1, j), getPoint(i, j + 1)), getPoint(i - 1, j), mock(getPoint(i - 1, j + 1), getPoint(i, j + 1), getPoint(i - 1, j)))));
		// }
		// for (int i = -1; i >= -halfHeight - extraHeight; i--) {
		// put(i, 0, createIndexedPoint(extrapole(getPoint(i + 2, 0), getPoint(i + 1, 0))));
		// for (int j = halfWidth + 1; j <= halfWidth + extraWidth; j++)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j - 1), mock(getPoint(i + 1, j - 1), getPoint(i + 1, j), getPoint(i, j - 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j - 1), getPoint(i, j - 1), getPoint(i + 1, j)))));
		// for (int j = -halfWidth - 1; j >= -halfWidth - extraWidth; j--)
		// put(i, j, createIndexedPoint(intersectExtra(getPoint(i, j + 1), mock(getPoint(i + 1, j + 1), getPoint(i + 1, j), getPoint(i, j + 1)), getPoint(i + 1, j), mock(getPoint(i + 1, j + 1), getPoint(i, j + 1), getPoint(i + 1, j)))));
		// }

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

	private IndexedPoint createIndexedPoint(Point point) {
		IndexedPoint indexedPoint = new IndexedPoint(pointIndex.size(), point);
		pointIndex.add(indexedPoint);
		return indexedPoint;
	}

	private Point intersect(Point hPoint, Point vPoint) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint
		double xDiff = xDiff(hPoint, vPoint);
		double yDiff = yDiff(vPoint, hPoint);
		while (Math.abs(xDiff) > 1 || Math.abs(yDiff) > 1) {
			xDiff = xDiff(hPoint, vPoint);
			yDiff = yDiff(vPoint, hPoint);
			hPoint = horizontalMove(hPoint, xDiff);
			vPoint = verticalMove(vPoint, yDiff);
		}
		if (Math.abs(xDiff) <= 1 && Math.abs(yDiff) <= 1)
			return new Point(0.5 * (hPoint.x + vPoint.x), 0.5 * (hPoint.y + vPoint.y));
		throw new IllegalStateException(xDiff + " " + yDiff);
	}

	private double xDiff(Point pt1, Point pt2) {
		return pt2.x - pt1.x;
	}

	private double yDiff(Point pt1, Point pt2) {
		return pt2.y - pt1.y;
	}

	private Point verticalMove(Point startingPoint, double deltaY) {
		if (deltaY == 0)
			return startingPoint;
		double dY = Math.max(1, deltaY / 2) * Math.signum(deltaY);
		double x = startingPoint.x, y = startingPoint.y;
		while (Math.abs(y - startingPoint.y - deltaY) >= 1) {
			double dX = dY / Math.tan(interpolator.interpolateVerticals(x - xBorder, y - yBorder) + Math.PI / 2);
			if (!Double.isFinite(dX))
				dX = 0;
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	private Point horizontalMove(Point startingPoint, double deltaX) {
		if (deltaX == 0)
			return startingPoint;
		double dX = Math.max(1, deltaX / 2) * Math.signum(deltaX);
		double x = startingPoint.x, y = startingPoint.y;
		while (Math.abs(x - startingPoint.x - deltaX) >= 1) {
			double dY = Math.tan(interpolator.interpolateHorizontals(x - xBorder, y - yBorder)) * dX;
			if (!Double.isFinite(dY))
				dY = 0;
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	// points = new Point[2 * halfHeight + 1][2 * halfWidth + 1];
	public void put(int i, int j, IndexedPoint point) {
		System.out.println(i + " " + j);
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