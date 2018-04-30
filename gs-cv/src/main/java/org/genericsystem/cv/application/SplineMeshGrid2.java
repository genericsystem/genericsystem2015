package org.genericsystem.cv.application;

import org.apache.commons.math3.analysis.FunctionUtils;
import org.apache.commons.math3.analysis.differentiation.UnivariateDifferentiableFunction;
import org.apache.commons.math3.analysis.function.Identity;
import org.apache.commons.math3.analysis.function.Minus;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.apache.commons.math3.analysis.solvers.BisectionSolver;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class SplineMeshGrid2 {

	private static final Logger logger = LoggerFactory.getLogger(MeshGrid.class);

	private final double deltaX, deltaY;
	private final int xBorder;
	private final int yBorder;
	private final Mat image;
	private final List<PolynomialSplineFunction> vLines;
	private final List<PolynomialSplineFunction> hLines;

	private final Point[][] points;
	private final int halfWidth;
	private final int halfHeight;

	public SplineMeshGrid2(List<PolynomialSplineFunction> vLines, List<PolynomialSplineFunction> hLines, double deltaX, double deltaY, Mat image) {
		this.vLines = vLines;
		this.hLines = hLines;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		this.image = new Mat();
		this.xBorder = 2 * (int) deltaX;
		this.yBorder = 2 * (int) deltaY;
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_REPLICATE);
		points = new Point[vLines.size()][hLines.size()];
		halfHeight = vLines.size() / 2;
		halfWidth = hLines.size() / 2;
	}

	public Point[][] build() {
		// assert hLines.size() % 2 == 1;
		// assert vLines.size() % 2 == 1;
		Point imgCenter = new Point(image.width() / 2 + xBorder, image.height() / 2 + yBorder);

		points[0 + halfHeight][0 + halfWidth] = imgCenter;
		for (int j = 1; j < halfWidth - 1; j++)
			points[0 + halfHeight][j + halfWidth] = addRight(0, j);
		for (int j = -1; j >= -halfWidth; j--)
			points[0 + halfHeight][j + halfWidth] = addLeft(0, j);
		for (int i = 1; i < halfHeight - 1; i++) {
			points[i + halfHeight][0 + halfWidth] = addBottom(i, 0);
			for (int j = 1; j < halfWidth - 1; j++)
				points[i + halfHeight][j + halfWidth] = addRightBottom(i, j);
			for (int j = -1; j >= -halfWidth; j--)
				points[i + halfHeight][j + halfWidth] = addLeftBottom(i, j);
		}
		for (int i = -1; i >= -halfHeight; i--) {
			points[i + halfHeight][0 + halfWidth] = addTop(i, 0);
			for (int j = 1; j < halfWidth - 1; j++)
				points[+halfHeight][j + halfWidth] = addRightTop(i, j);
			for (int j = -1; j >= -halfWidth; j--)
				points[i + halfHeight][j + halfWidth] = addLeftTop(i, j);
		}
		return points;
	}

	private Point addTop(int i, int j) {
		return verticalMove(i, points[i + 1 + halfHeight][j + halfWidth], -deltaY);
	}

	private Point addBottom(int i, int j) {
		System.out.println("ADDBottom " + i + " " + j);
		return verticalMove(i, points[i - 1 + halfHeight][j + halfWidth], deltaY);
	}

	private Point addLeft(int i, int j) {
		return horizontalMove(j, points[i + halfHeight][j + 1 + halfWidth], -deltaX);
	}

	private Point addRight(int i, int j) {
		return horizontalMove(j, points[i + halfHeight][j - 1 + halfWidth], deltaX);
	}

	private Point addLeftTop(int i, int j) {
		return intersectLeftTop(i, j, points[i + halfHeight][j + 1 + halfWidth], points[i + 1 + halfHeight][j + halfWidth]);
	}

	private Point addRightTop(int i, int j) {
		return intersectRightTop(i, j, points[i + halfHeight][j - 1 + halfWidth], points[i + 1 + halfHeight][j + halfWidth]);
	}

	private Point addLeftBottom(int i, int j) {
		return intersectLeftBottom(i, j, points[i + halfHeight][j + 1 + halfWidth], points[i - 1 + halfHeight][j + halfWidth]);

	}

	private Point addRightBottom(int i, int j) {
		System.out.println("ADDRightBottom " + i + " " + j);
		return intersectRightBottom(i, j, points[i + halfHeight][j - 1 + halfWidth], points[i - 1 + halfHeight][j + halfWidth]);

	}

	private Point intersectLeftTop(int hLine, int vLine, Point hPoint, Point vPoint) {
		return intersect(hLine, vLine, hPoint, vPoint, 0, hPoint.x - xBorder, 0, vPoint.y - yBorder);
	}

	private Point intersectLeftBottom(int hLine, int vLine, Point hPoint, Point vPoint) {
		return intersect(hLine, vLine, hPoint, vPoint, 0, hPoint.x - xBorder, vPoint.y - yBorder, image.height() - 1 - 2 * yBorder);
	}

	private Point intersectRightTop(int hLine, int vLine, Point hPoint, Point vPoint) {
		return intersect(hLine, vLine, hPoint, vPoint, hPoint.x - xBorder, image.width() - 1 - 2 * xBorder, 0, vPoint.y - yBorder);
	}

	private Point intersectRightBottom(int hLine, int vLine, Point hPoint, Point vPoint) {
		return intersect(hLine, vLine, hPoint, vPoint, hPoint.x - xBorder, image.width() - 1 - 2 * xBorder, vPoint.y - yBorder, image.height() - 1 - 2 * yBorder);
	}

	private Point intersect(int hLine, int vLine, Point hPoint, Point vPoint, double minX, double maxX, double minY, double maxY) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint

		System.out.println("minX : " + minX + " maxX : " + maxX);
		System.out.println("minY : " + minY + " maxY : " + maxY);

		PolynomialSplineFunction hSpline = hLines.get(hLine + halfHeight);
		PolynomialSplineFunction vSpline = vLines.get(vLine + halfWidth);
		if (hSpline.isValidPoint(minX) && hSpline.isValidPoint(maxX)) {

			double yminX = hSpline.value(minX);
			double ymaxX = hSpline.value(maxX);

			if (vSpline.isValidPoint(yminX) && vSpline.isValidPoint(ymaxX)) {
				UnivariateDifferentiableFunction f = FunctionUtils.add(FunctionUtils.compose(new UnivariateDifferentiableFunction[] { vSpline, hSpline }), FunctionUtils.compose(new UnivariateDifferentiableFunction[] { new Minus(), new Identity() }));
				try {
					double x = new BisectionSolver().solve(100, f, minX, maxX);
					Point result = new Point(x + xBorder, hSpline.value(x) + yBorder);
					System.out.println(hLine + " " + vLine + " " + result);
					return result;
				} catch (Exception ignore) {

				}
			}

		}
		if (vSpline.isValidPoint(minY) && vSpline.isValidPoint(maxY)) {
			double xminY = vSpline.value(minY);
			double xmaxY = vSpline.value(maxY);
			if (hSpline.isValidPoint(xminY) && hSpline.isValidPoint(xmaxY)) {
				UnivariateDifferentiableFunction f = FunctionUtils.add(FunctionUtils.compose(new UnivariateDifferentiableFunction[] { hSpline, vSpline }), FunctionUtils.compose(new UnivariateDifferentiableFunction[] { new Minus(), new Identity() }));
				try {
					double y = new BisectionSolver().solve(100, f, minY, maxY);
					Point result = new Point(vSpline.value(y) + xBorder, y + yBorder);
					System.out.println(hLine + " " + vLine + " " + result);
					return result;
				} catch (Exception ignore) {
					System.out.println(hLine + " " + vLine + " " + " Double exception");
					return null;
				}
			} else {
				System.out.println(hLine + " " + vLine + " None1 ");
				return null;
			}
		} else {
			System.out.println(hLine + " " + vLine + " None2 ");
			return null;
		}
	}

	private Point verticalMove(int vLine, Point startingPoint, double deltaY) {
		System.out.println(vLine + " " + halfHeight);
		if (!vLines.get(vLine + halfHeight).isValidPoint(startingPoint.y + deltaY - yBorder)) {
			System.out.println(0 + " " + vLine + " None ");
			return null;
		}
		Point result = new Point(vLines.get(vLine + halfHeight).value(startingPoint.y + deltaY - yBorder) + xBorder, startingPoint.y + deltaY - yBorder);
		System.out.println(0 + " " + vLine + " " + result);
		return result;
	}

	private Point horizontalMove(int hLine, Point startingPoint, double deltaX) {
		if (!hLines.get(hLine + halfWidth).isValidPoint(startingPoint.x - xBorder + deltaX)) {
			System.out.println(hLine + " " + 0 + " None ");
			return null;
		}

		Point result = new Point(startingPoint.x + deltaX, hLines.get(hLine + halfWidth).value(startingPoint.x - xBorder + deltaX) + yBorder);
		System.out.println(hLine + " " + 0 + " " + result);
		return result;
	}

	// -----------------------------------------------------------------------------------------

	private Size getOldSize() {
		return new Size(image.width() - 2 * xBorder, image.height() - 2 * yBorder);
	}

	// public Mat drawOnCopy(Scalar color) {
	// Mat clone = image.clone();
	// mesh.values().forEach(p -> drawPolygon(clone, p, color));
	// return clone;
	// }

	// Returns true if at least a corner of the polygon is in the image proper,
	// that is, not outside and not in one of the borders whose size is defined by xBorder and yBorder.
	private boolean inImageBorders(Point[] p) {
		// Array of points: top left, top right, bottom right, bottom left.
		for (Point pt : p)
			if (!inImageBorders(pt))
				return false;
		return true;
	}

	private boolean inImageBorders(Point p) {
		return p.x >= 0 && p.x < image.width() && p.y >= 0 && p.y < image.height();
	}

	protected Mat dewarpPolygon(Point[] polygon, Rect subImageRect, double rectHeight, double rectWidth) {
		Point warpedTopLeft = changeOrigin(subImageRect, polygon[0]);
		Point warpedTopRight = changeOrigin(subImageRect, polygon[1]);
		Point warpedBottomRight = changeOrigin(subImageRect, polygon[2]);
		Point warpedBottomLeft = changeOrigin(subImageRect, polygon[3]);
		Point dewarpedTopLeft = new Point(0, 0);
		Point dewarpedTopRight = new Point(rectWidth, 0);
		Point dewarpedBottomRight = new Point(rectWidth, rectHeight);
		Point dewarpedBottomLeft = new Point(0, rectHeight);
		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(warpedTopLeft, warpedTopRight, warpedBottomRight, warpedBottomLeft), new MatOfPoint2f(dewarpedTopLeft, dewarpedTopRight, dewarpedBottomRight, dewarpedBottomLeft));
	}

	protected Point changeOrigin(Rect subImageRect, Point point) {
		return new Point(point.x - subImageRect.x, point.y - subImageRect.y);
	}

	protected void drawPolygon(Mat image, Point[] polygon, Scalar color) {
		Point topLeft = polygon[0];
		Point topRight = polygon[1];
		Point bottomRight = polygon[2];
		Point bottomLeft = polygon[3];
		Imgproc.line(image, topLeft, topRight, color);
		Imgproc.line(image, topRight, bottomRight, color);
		Imgproc.line(image, bottomRight, bottomLeft, color);
		Imgproc.line(image, bottomLeft, topLeft, color);
	}

}
