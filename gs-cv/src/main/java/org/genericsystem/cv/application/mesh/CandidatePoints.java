package org.genericsystem.cv.application.mesh;

import org.genericsystem.cv.application.Interpolator;
import org.opencv.core.Point;

public class CandidatePoints extends Points {

	private final Interpolator interpolator;
	private final double deltaX;
	private final double deltaY;

	public CandidatePoints(Point imgCenter, int halfWidth, int halfHeight, double deltaX, double deltaY, int xBorder, int yBorder, Interpolator interpolator) {
		super(xBorder, yBorder);
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		put(0, 0, createIndexedPoint(imgCenter));
		for (int j = 1; j <= halfWidth; j++)
			put(0, j, createIndexedPoint(horizontalMove(getPoint(0, j - 1), getWidthCoeff(j + halfWidth - 1))));
		for (int j = -1; j >= -halfWidth; j--)
			put(0, j, createIndexedPoint(horizontalMove(getPoint(0, j + 1), -getWidthCoeff(j + halfWidth))));
		for (int i = 1; i <= halfHeight; i++) {
			put(i, 0, createIndexedPoint(verticalMove(getPoint(i - 1, 0), getHeightCoeff(i + halfHeight - 1))));
			for (int j = 1; j <= halfWidth; j++)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j - 1), getPoint(i - 1, j))));
			for (int j = -1; j >= -halfWidth; j--)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j + 1), getPoint(i - 1, j))));
		}
		for (int i = -1; i >= -halfHeight; i--) {
			put(i, 0, createIndexedPoint(verticalMove(getPoint(i + 1, 0), -getHeightCoeff(i + halfHeight))));
			for (int j = 1; j <= halfWidth; j++)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j - 1), getPoint(i + 1, j))));
			for (int j = -1; j >= -halfWidth; j--)
				put(i, j, createIndexedPoint(intersect(getPoint(i, j + 1), getPoint(i + 1, j))));
		}
	}

	double getWidthCoeff(int j) {
		return deltaX;
	}

	double getHeightCoeff(int i) {
		return deltaY;
	}

	public Interpolator getInterpolator() {
		return interpolator;
	}

	private Point verticalMove(Point startingPoint, double deltaY) {
		if (deltaY == 0)
			return startingPoint;
		double dY = Math.signum(deltaY);
		double x = startingPoint.x, y = startingPoint.y;
		while (Math.abs(y - startingPoint.y - deltaY) >= 1) {
			double dX = interpolator.interpolateVerticals(x - xBorder, y - yBorder) * dY;
			assert Double.isFinite(dX) : interpolator.interpolateVerticals(x - xBorder, y - yBorder);
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	private Point horizontalMove(Point startingPoint, double deltaX) {
		if (deltaX == 0)
			return startingPoint;
		double dX = Math.signum(deltaX);
		double x = startingPoint.x, y = startingPoint.y;
		while (Math.abs(x - startingPoint.x - deltaX) >= 1) {
			double dY = interpolator.interpolateHorizontals(x - xBorder, y - yBorder) * dX;
			assert Double.isFinite(dY) : interpolator.interpolateHorizontals(x - xBorder, y - yBorder);
			x += dX;
			y += dY;
		}
		return new Point(x, y);
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

}
