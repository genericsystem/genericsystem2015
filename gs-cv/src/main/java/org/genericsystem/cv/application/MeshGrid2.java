package org.genericsystem.cv.application;

import org.genericsystem.cv.Svd;
import org.genericsystem.cv.application.MeshGrid.Key;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Point3;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

public class MeshGrid2 {

	private static final Logger logger = LoggerFactory.getLogger(MeshGrid.class);

	private final double deltaX, deltaY;
	private final int xBorder;
	private final int yBorder;
	private final Mat image;

	private final int halfWidth;
	private final int halfHeight;

	private Interpolator interpolator;
	private Point[][] points;
	// protected Point[][][] mesh;
	protected Mesh mesh = new Mesh();

	private static class Mesh {

		private HashMap<Key, Point[]> internal = new HashMap<>();

		public void put(int i, int j, Point... points) {
			internal.put(new Key(i, j), points);
		}

		public Point[] get(int i, int j) {
			return internal.get(new Key(i, j));
			// return internal.get(new Key(i + halfHeight, j + halfWidth));
		}

		public int size() {
			return internal.size();
		}

		public Collection<Point[]> values() {
			return internal.values();
		}

		public Set<Entry<Key, Point[]>> entrySet() {
			return internal.entrySet();
		}
	}

	public MeshGrid2(int halfWidth, int halfHeight, Interpolator interpolator, double deltaX, double deltaY, Mat image) {
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		this.image = new Mat();
		xBorder = 2 * (int) deltaX;
		yBorder = 2 * (int) deltaY;
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_REPLICATE);
		points = new Point[2 * halfHeight + 1][2 * halfWidth + 1];
		this.halfHeight = halfHeight;
		this.halfWidth = halfWidth;
		// this.mesh = new Point[2 * halfHeight][2 * halfWidth][4];
		buildGrid();
		buildMesh();
	}

	public void buildGrid() {
		Point imgCenter = new Point(image.width() / 2, image.height() / 2);
		points[0 + halfHeight][0 + halfWidth] = imgCenter;
		for (int j = 1; j <= halfWidth; j++)
			points[0 + halfHeight][j + halfWidth] = addRight(0, j);
		for (int j = -1; j >= -halfWidth; j--)
			points[0 + halfHeight][j + halfWidth] = addLeft(0, j);
		for (int i = 1; i <= halfHeight; i++) {
			points[i + halfHeight][0 + halfWidth] = addBottom(i, 0);
			for (int j = 1; j <= halfWidth; j++)
				points[i + halfHeight][j + halfWidth] = addRightBottom(i, j);
			for (int j = -1; j >= -halfWidth; j--)
				points[i + halfHeight][j + halfWidth] = addLeftBottom(i, j);
		}
		for (int i = -1; i >= -halfHeight; i--) {
			points[i + halfHeight][0 + halfWidth] = addTop(i, 0);
			for (int j = 1; j <= halfWidth; j++)
				points[i + halfHeight][j + halfWidth] = addRightTop(i, j);
			for (int j = -1; j >= -halfWidth; j--)
				points[i + halfHeight][j + halfWidth] = addLeftTop(i, j);
		}
	}

	private void buildMesh() {
		for (int i = -halfHeight + 1; i <= halfHeight; i++)
			for (int j = -halfWidth + 1; j <= halfWidth; j++) {
				Point leftTop = points[i + halfHeight - 1][j + halfWidth - 1];
				Point rightTop = points[i + halfHeight - 1][j + halfWidth];
				Point rightBottom = points[i + halfHeight][j + halfWidth];
				Point leftBottom = points[i + halfHeight][j + halfWidth - 1];
				assert leftTop != null : i + " " + j;
				assert rightTop != null : i + " " + j;
				assert rightBottom != null : i + " " + j;
				assert leftBottom != null : i + " " + j;
				mesh.put(i - 1, j - 1, leftTop, rightTop, rightBottom, leftBottom);
				// mesh[i + halfHeight - 1][j + halfWidth - 1] = new Point[] { leftTop, rightTop, rightBottom, leftBottom };
			}
	}

	private Point addTop(int i, int j) {
		return verticalMove(points[i + 1 + halfHeight][j + halfWidth], -deltaY);
	}

	private Point addBottom(int i, int j) {
		return verticalMove(points[i - 1 + halfHeight][j + halfWidth], deltaY);
	}

	private Point addLeft(int i, int j) {
		return horizontalMove(points[i + halfHeight][j + 1 + halfWidth], -deltaX);
	}

	private Point addRight(int i, int j) {
		return horizontalMove(points[i + halfHeight][j - 1 + halfWidth], deltaX);
	}

	private Point addLeftTop(int i, int j) {
		return intersect(points[i + halfHeight][j + 1 + halfWidth], points[i + 1 + halfHeight][j + halfWidth]);
	}

	private Point addRightTop(int i, int j) {
		return intersect(points[i + halfHeight][j - 1 + halfWidth], points[i + 1 + halfHeight][j + halfWidth]);
	}

	private Point addLeftBottom(int i, int j) {
		return intersect(points[i + halfHeight][j + 1 + halfWidth], points[i - 1 + halfHeight][j + halfWidth]);
	}

	private Point addRightBottom(int i, int j) {
		return intersect(points[i + halfHeight][j - 1 + halfWidth], points[i - 1 + halfHeight][j + halfWidth]);
	}

	private Point intersect(Point hPoint, Point vPoint) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint
		Point intersection = null;
		double xDiff = xDiff(hPoint, vPoint);
		double yDiff = yDiff(vPoint, hPoint);
		while (Math.abs(xDiff) > 1 || Math.abs(yDiff) > 1) {
			xDiff = xDiff(hPoint, vPoint);
			yDiff = yDiff(vPoint, hPoint);
			hPoint = horizontalMove(hPoint, xDiff);
			vPoint = verticalMove(vPoint, yDiff);
		}
		if (Math.abs(xDiff) <= 1 && Math.abs(yDiff) <= 1) {
			intersection = new Point(0.5 * (hPoint.x + vPoint.x), 0.5 * (hPoint.y + vPoint.y));
			return intersection;
		}
		throw new IllegalStateException(xDiff + " " + yDiff);
	}

	private double xDiff(Point pt1, Point pt2) {
		return pt2.x - pt1.x;
	}

	private double yDiff(Point pt1, Point pt2) {
		return pt2.y - pt1.y;
	}

	private Point verticalMove(Point startingPoint, double deltaY) {
		assert Double.isFinite(startingPoint.x);
		assert Double.isFinite(startingPoint.y);
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
		assert Double.isFinite(x);
		assert Double.isFinite(y);
		return new Point(x, y);
	}

	private Point horizontalMove(Point startingPoint, double deltaX) {
		assert Double.isFinite(startingPoint.x);
		assert Double.isFinite(startingPoint.y);
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
		assert Double.isFinite(x);
		assert Double.isFinite(y);
		return new Point(x, y);
	}

	// -----------------------------------------------------------------------------------------

	private Size getOldSize() {
		return new Size(image.width() - 2 * xBorder, image.height() - 2 * yBorder);
	}

	protected Rect subImageRect(int i, int j) {
		Point[] polygon = mesh.get(i, j);
		assert polygon != null : i + " " + j;
		Point warpedTopLeft = polygon[0];
		Point warpedTopRight = polygon[1];
		Point warpedBottomRight = polygon[2];
		Point warpedBottomLeft = polygon[3];
		double xMin, xMax, yMin, yMax;
		xMin = Math.min(warpedTopLeft.x, warpedBottomLeft.x);
		xMax = Math.max(warpedBottomRight.x, warpedTopRight.x);
		yMin = Math.min(warpedTopRight.y, warpedTopLeft.y);
		yMax = Math.max(warpedBottomLeft.y, warpedBottomRight.y);
		return new Rect(new Point(xMin, yMin), new Point(xMax, yMax));
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

	protected void drawPolygon(Mat image, Point[] polygon, Scalar meshColor, Scalar ptsColor) {
		Point topLeft = polygon[0];
		Point topRight = polygon[1];
		Point bottomRight = polygon[2];
		Point bottomLeft = polygon[3];
		Imgproc.circle(image, topLeft, 5, ptsColor, -1);
		Imgproc.circle(image, topRight, 5, ptsColor, -1);
		Imgproc.circle(image, bottomRight, 5, ptsColor, -1);
		Imgproc.circle(image, bottomLeft, 5, ptsColor, -1);
		Imgproc.line(image, topLeft, topRight, meshColor);
		Imgproc.line(image, topRight, bottomRight, meshColor);
		Imgproc.line(image, bottomRight, bottomLeft, meshColor);
		Imgproc.line(image, bottomLeft, topLeft, meshColor);
	}

	public Mat drawOnCopy(Scalar meshColor, Scalar ptsColor) {
		Mat clone = image.clone();
		for (int i = -halfHeight; i < halfHeight; i++)
			for (int j = -halfWidth; j < halfWidth; j++)
				drawPolygon(clone, mesh.get(i, j), meshColor, ptsColor);
		return clone;
	}

	// --------------------------------------------------

	private List<Point3> points3D;

	public Map<Key, Point3[]> toPoint3d() {
		int[][] rects = new int[mesh.size()][4];
		List<Point> pointIndex = Arrays.stream(points).flatMap(Arrays::stream).collect(Collectors.toList());
		List<Point[]> meshPoints = new ArrayList<>(mesh.values());
		for (int i = 0; i < rects.length; i++)
			for (int j = 0; j < 4; j++)
				rects[i][j] = pointIndex.indexOf(meshPoints.get(i)[j]);

		points3D = Svd.solve(pointIndex, rects);
		Map<Key, Point3[]> mesh3D = new HashMap<>();
		for (Entry<Key, Point[]> entry : mesh.entrySet()) {
			Point3[] para3D = new Point3[4];
			for (int j = 0; j < 4; j++)
				para3D[j] = points3D.get(pointIndex.indexOf(entry.getValue()[j]));
			mesh3D.put(entry.getKey(), para3D);
		}
		return mesh3D;
	}

	public Mat draw3Dsurface(Map<Key, Point3[]> mesh3D, Scalar colorStart, Scalar colorEnd) {
		double xMin = points3D.stream().mapToDouble(p -> p.x).min().getAsDouble();
		double yMin = points3D.stream().mapToDouble(p -> p.y).min().getAsDouble();
		double zMin = points3D.stream().mapToDouble(p -> p.z).min().getAsDouble();
		double xMax = points3D.stream().mapToDouble(p -> p.x).max().getAsDouble();
		double yMax = points3D.stream().mapToDouble(p -> p.y).max().getAsDouble();
		double zMax = points3D.stream().mapToDouble(p -> p.z).max().getAsDouble();

		int newWidth = image.width();
		int newHeight = (int) Math.ceil((yMax - yMin) * image.width() / (xMax - xMin));
		Mat result = new Mat(newHeight, newWidth, CvType.CV_8UC3, new Scalar(0, 0, 0));
		List<Point3> normalizedPoints = points3D.stream().map(p -> normalize(p, 0, result.width() - 1, 0, result.height() - 1, xMin, xMax, yMin, yMax)).collect(Collectors.toList());
		Map<Key, Point3[]> normalizedMesh = normalize(mesh3D, normalizedPoints);
		for (Point3[] p : normalizedMesh.values()) {
			Point[] p2 = new Point[] { new Point(p[0].x, p[0].y), new Point(p[1].x, p[1].y), new Point(p[2].x, p[2].y), new Point(p[3].x, p[3].y) };
			double lambda = (p[0].z - zMin) / (zMax - zMin);
			Scalar color = combine(colorStart, colorEnd, lambda);
			drawPolygon(result, p2, color, color);
		}
		return result;
	}

	private Map<Key, Point3[]> normalize(Map<Key, Point3[]> mesh, List<Point3> newPoints) {
		Map<Key, Point3[]> newMesh = new HashMap<>();
		for (Entry<Key, Point3[]> entry : mesh.entrySet())
			newMesh.put(entry.getKey(), exchangePoints(entry.getValue(), points3D, newPoints));
		return newMesh;
	}

	private Point3[] exchangePoints(Point3[] pts, List<Point3> oldPts, List<Point3> newPts) {
		Point3[] result = new Point3[pts.length];
		for (int i = 0; i < pts.length; i++)
			result[i] = newPts.get(oldPts.indexOf(pts[i]));
		return result;
	}

	private Point3 normalize(Point3 p, double xMin, double xMax, double yMin, double yMax, double xMinOrig, double xMaxOrig, double yMinOrig, double yMaxOrig) {
		return new Point3(normalize(p.x, xMin, xMax, xMinOrig, xMaxOrig), normalize(p.y, yMin, yMax, yMinOrig, yMaxOrig), p.z);
	}

	private double normalize(double x, double xMin, double xMax, double xMinOrig, double xMaxOrig) {
		return (xMax - xMin) * (x - xMinOrig) / (xMaxOrig - xMinOrig) + xMin;
	}

	private Scalar combine(Scalar colorStart, Scalar colorEnd, double lambda) {
		double[] c1 = colorStart.val;
		double[] c2 = colorEnd.val;
		double[] c = new double[c1.length];
		for (int i = 0; i < c.length; i++)
			c[i] = (1 - lambda) * c1[i] + lambda * c2[i];
		return new Scalar(c);
	}

	public Mat dewarp3D(Map<Key, Point3[]> mesh3D) {
		int nVerts = halfWidth * 2;
		int nLines = halfHeight * 2;
		// Average width of the 3D edges for each column.
		double[] widths = new double[nVerts];
		for (int j = 0; j < widths.length; j++) {
			double sum = 0;
			for (int i = 0; i < nLines - 1; i++) {
				Point3[] para = mesh3D.get(new Key(i - halfHeight, j - halfWidth));
				assert para != null : "i:" + (i - halfHeight) + " j: " + (j - halfWidth);
				sum += euclideanDistance(para[0], para[1]);
			}
			// Last line, bottom edge.
			Point3[] para = mesh3D.get(new Key(nLines - 1 - halfHeight, j - halfWidth));
			sum += euclideanDistance(para[2], para[3]);
			widths[j] = sum / nLines;
		}

		// Average height of the 3D edges for each line.
		double[] heights = new double[nLines];
		for (int i = 0; i < heights.length; i++) {
			double sum = 0;
			for (int j = 0; j < nVerts - 1; j++) {
				Point3[] para = mesh3D.get(new Key(i - halfHeight, j - halfWidth));
				sum += euclideanDistance(para[0], para[3]);
			}
			// Last column, right edge.
			Point3[] para = mesh3D.get(new Key(i - halfHeight, nVerts - 1 - halfWidth));
			sum += euclideanDistance(para[1], para[2]);

			heights[i] = sum / nVerts;
		}

		Mat enlargedImage = new Mat(image.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));

		double height = 2 * Math.min(DoubleStream.of(heights).limit(halfHeight).sum(), DoubleStream.of(heights).skip(halfHeight).sum());
		double width = 2 * Math.min(DoubleStream.of(widths).limit(halfWidth).sum(), DoubleStream.of(widths).skip(halfWidth).sum());

		double coefHeight = getOldSize().height / height;
		for (int i = 0; i < heights.length; i++)
			heights[i] *= coefHeight;

		double coefWidth = getOldSize().width / width;
		for (int i = 0; i < widths.length; i++)
			widths[i] *= coefWidth;

		System.out.println(Arrays.toString(heights));
		System.out.println(Arrays.toString(widths));
		System.out.println(DoubleStream.of(heights).sum());
		System.out.println(DoubleStream.of(widths).sum());

		double y = enlargedImage.height() / 2;
		for (int i = 0; i < halfHeight; i++) {
			double x = enlargedImage.width() / 2;
			for (int j = 0; j < halfWidth; j++) {
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(enlargedImage, i, j, x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}
			x = enlargedImage.width() / 2;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(enlargedImage, i, j, x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
			y += heights[i + halfHeight];
		}
		y = enlargedImage.height() / 2;
		for (int i = -1; i >= -halfHeight; i--) {
			y -= heights[i + halfHeight];
			double x = enlargedImage.width() / 2;
			for (int j = 0; j < halfWidth; j++) {
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(enlargedImage, i, j, x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}
			x = enlargedImage.width() / 2;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(enlargedImage, i, j, x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
		}
		return new Mat(enlargedImage, new Rect(new Point(xBorder, yBorder), new Point(enlargedImage.width() - xBorder, enlargedImage.height() - yBorder)));
	}

	public Mat dewarp() {
		double rectHeight = getOldSize().height / (2 * halfHeight);
		double rectWidth = getOldSize().width / (2 * halfWidth);
		Mat dewarpedImage = new Mat(getOldSize(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int i = -halfHeight; i < halfHeight; i++)
			for (int j = -halfWidth; j < halfWidth; j++)
				deWarp(dewarpedImage, i, j, (j + halfWidth) * rectWidth, (i + halfHeight) * rectHeight, rectWidth, rectHeight);
		return dewarpedImage;
	}

	private void deWarp(Mat dewarpedImage, int i, int j, double x, double y, double rectWidth, double rectHeight) {
		if (inBordedImage(mesh.get(i, j))) {
			Rect subImageRect = subImageRect(i, j);
			if (!subImageRect.empty()) {
				Mat homography = dewarpPolygon(mesh.get(i, j), subImageRect, rectHeight, rectWidth);
				Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectWidth, y + rectHeight)));
				Mat subImage = new Mat(image, subImageRect);
				Imgproc.warpPerspective(subImage, subDewarpedImage, homography, subDewarpedImage.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
				subImage.release();
				subDewarpedImage.release();
				homography.release();
			}
		}
	}

	private boolean inBordedImage(Point[] pts) {
		for (Point p : pts)
			if (p.x < 0 || p.x >= image.width() || p.y < 0 || p.y >= image.height())
				return false;
		return true;
	}

	private double euclideanDistance(Point3 p1, Point3 p2) {
		return Math.sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y) + (p2.z - p1.z) * (p2.z - p1.z));
	}

}
