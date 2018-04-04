package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.genericsystem.cv.Svd2;
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

public class MeshGrid {

	private static final Logger logger = LoggerFactory.getLogger(MeshGrid.class);
	private final Size kSize;
	public List<Point> points = new ArrayList<>();
	// Array of points: top left, top right, bottom right, bottom left.
	private Map<Key, Point[]> mesh = new HashMap<>(); // les mêmes polygones mais en i, j
	private Interpolator interpolator;
	public double deltaX, deltaY; // déplacement d'un polygone
	private final int xBorder;
	private final int yBorder;
	private final Mat image;

	private int nbIter; // nombre d'itérations à chaque déplacement

	public MeshGrid(Size kSize, Interpolator interpolator, double deltaX, double deltaY, Mat image) {
		this.kSize = kSize;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		this.image = image;
		xBorder = 2 * (int) deltaX;
		yBorder = 2 * (int) deltaY;
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_CONSTANT, new Scalar(255, 255, 255));
		nbIter = (int) Math.round(deltaY); // avance d'un pixel à chaque itération
	}

	private int[][] toRectIndices() {
		int[][] rects = new int[mesh.size()][4];
		List<Point[]> meshPoints = new ArrayList<>(mesh.values());
		for (int i = 0; i < rects.length; i++)
			for (int j = 0; j < 4; j++)
				rects[i][j] = points.indexOf(meshPoints.get(i)[j]);
		return rects;
	}

	private Map<Key, Point3[]> toPoint3d() {
		List<Point3> points3D = Svd2.solve(points, toRectIndices());
		Map<Key, Point3[]> mesh3D = new HashMap<>();
		for (Entry<Key, Point[]> entry : mesh.entrySet()) {
			Point3[] para3D = new Point3[4];
			for (int j = 0; j < 4; j++)
				para3D[j] = points3D.get(points.indexOf(entry.getValue()[j]));
			mesh3D.put(entry.getKey(), para3D);
		}
		return mesh3D;
	}

	public void build() {
		Point imgCenter = new Point(image.width() / 2, image.height() / 2);
		addFirstPoly(imgCenter);
		for (int i = 0; i <= kSize.height; i++) {
			for (int j = 0; j <= (int) kSize.width; j++)
				if (i != 0 || j != 0)
					addPolygon(i, j);
			for (int j = -1; j >= -(int) kSize.width; j--)
				addPolygon(i, j);
		}
		for (int i = -1; i >= -kSize.height; i--) {
			for (int j = 0; j <= (int) kSize.width; j++)
				addPolygon(i, j);
			for (int j = -1; j >= -(int) kSize.width; j--)
				addPolygon(i, j);
		}
	}

	public void draw(Mat img, Scalar color) {
		mesh.values().forEach(p -> drawPolygon(img, p, color));
	}

	public Mat dewarp2() {
		Map<Key, Point3[]> mesh3D = toPoint3d();

		// Average width of the 3D edges for each column.
		int[] widths = new int[2 * (int) kSize.width + 1];
		for (int j = 0; j < widths.length; j++) {
			double sum = 0;
			for (int i = (int) -kSize.height; i <= kSize.height; i++) {
				Point3[] para = mesh3D.get(new Key(i, j - (int) kSize.width));
				sum += euclideanDistance(para[0], para[1]);
			}
			// Last line, bottom edge.
			Point3[] para = mesh3D.get(new Key((int) kSize.height, j - (int) kSize.width));
			sum += euclideanDistance(para[2], para[3]);
			widths[j] = (int) Math.round(sum / (2 * kSize.height + 2));
		}

		// Average height of the 3D edges for each line.
		int[] heights = new int[2 * (int) kSize.height + 1];
		for (int i = 0; i < heights.length; i++) {
			double sum = 0;
			for (int j = (int) -kSize.width; j <= kSize.width; j++) {
				Point3[] para = mesh3D.get(new Key(i - (int) kSize.height, j));
				sum += euclideanDistance(para[0], para[3]);
			}
			// Last column, right edge.
			Point3[] para = mesh3D.get(new Key(i - (int) kSize.height, (int) kSize.width));
			sum += euclideanDistance(para[1], para[2]);
			heights[i] = (int) Math.round(sum / (2 * kSize.width + 2));
		}

		// Rescaling ratio.
		int totalHeight = sum(heights, heights.length);
		int textSep = 20;
		double ratio = ((double) totalHeight / heights.length) / textSep;

		for (int i = 0; i < widths.length; i++)
			widths[i] /= ratio;

		for (int i = 0; i < heights.length; i++)
			heights[i] /= ratio;

		// New sizes
		int totalWidth = sum(widths, widths.length);
		totalHeight = sum(heights, heights.length);

		int rectHeight = totalHeight / heights.length;

		Mat dewarpedImage = new Mat(totalHeight + 1, totalWidth + 1, CvType.CV_8UC3, new Scalar(255, 255, 255));

		for (int i = (int) -kSize.height; i <= kSize.height; i++) {
			int x = 0;
			for (int j = (int) -kSize.width; j <= kSize.width; j++) {
				int wJ = j + (int) kSize.width;
				if (wJ > 0)
					x += widths[wJ - 1];
				if (inImageBorders(mesh.get(new Key(i, j)))) {
					int rectWidth = widths[wJ];
					Rect subImageRect = subImageRect(i, j);
					int y = (i + (int) kSize.height) * rectHeight;
					Mat homography = dewarpPolygon(mesh.get(new Key(i, j)), subImageRect, rectHeight, rectWidth);
					Rect dewarpedRect = new Rect(new Point(x, y), new Point(x + rectWidth, y + rectHeight));
					Mat subDewarpedImage = new Mat(dewarpedImage, dewarpedRect);
					Mat subImage = new Mat(image, subImageRect);
					Imgproc.warpPerspective(subImage, subDewarpedImage, homography, subDewarpedImage.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
					subImage.release();
					subDewarpedImage.release();
					homography.release();
				}
			}
		}
		return dewarpedImage;
	}

	private int sum(int[] array, int end) {
		int sum = 0;
		for (int i = 0; i < end; i++)
			sum += array[i];
		return sum;
	}

	// Returns true if at least a corner of the polygon is in the image proper,
	// that is, not outside and not in one of the borders whose size is defined by xBorder and yBorder.
	private boolean inImageBorders(Point[] p) {
		// Array of points: top left, top right, bottom right, bottom left.
		for (Point pt : p)
			if (inImageBorders(pt))
				return true;
		return false;
	}

	private boolean inImageBorders(Point p) {
		return p.x >= xBorder && p.x < image.width() - xBorder && p.y >= yBorder && p.y < image.height() - yBorder;
	}

	public double ratio(Point3[] parallelogram) {
		double width1 = euclideanDistance(parallelogram[0], parallelogram[1]);
		double width2 = euclideanDistance(parallelogram[2], parallelogram[3]);
		double height1 = euclideanDistance(parallelogram[0], parallelogram[3]);
		double height2 = euclideanDistance(parallelogram[2], parallelogram[1]);
		double width = (width1 + width2) / 2;
		double height = (height1 + height2) / 2;
		double ratio = width / height;
		return ratio;
	}

	private double euclideanDistance(Point3 p1, Point3 p2) {
		return Math.sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y) + (p2.z - p1.z) * (p2.z - p1.z));
	}

	public Mat dewarp() {
		int rectHeight = (int) Math.floor(image.height() / (2 * kSize.height + 1));
		int rectWidth = (int) Math.floor(image.width() / (2 * kSize.width + 1));

		Mat dewarpedImage = new Mat(image.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int i = (int) -kSize.height; i <= kSize.height; i++) {
			for (int j = (int) -kSize.width; j <= kSize.width; j++) {
				if (inImageBorders(mesh.get(new Key(i, j)))) {
					Rect subImageRect = subImageRect(i, j);
					Mat homography = dewarpPolygon(mesh.get(new Key(i, j)), subImageRect, rectHeight, rectWidth);
					int x = (j + (int) kSize.width) * rectWidth;
					int y = (i + (int) kSize.height) * rectHeight;
					assert x >= 0 && y >= 0 && ((x + rectWidth) < image.width()) && ((y + rectHeight) < image.height()) : "x: " + x + ", y: " + y + ", width: " + image.width() + ", height: " + image.height();
					Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectWidth, y + rectHeight)));
					Mat subImage = new Mat(image, subImageRect);
					Imgproc.warpPerspective(subImage, subDewarpedImage, homography, new Size(rectWidth, rectHeight), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
					subImage.release();
					subDewarpedImage.release();
					homography.release();
				}
			}
		}
		return dewarpedImage;
	}

	private Rect subImageRect(int iP, int jP) {
		Point[] polygon = mesh.get(new Key(iP, jP));
		assert polygon != null : iP + " " + jP;

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

	private Mat dewarpPolygon(Point[] polygon, Rect subImageRect, double rectHeight, double rectWidth) {
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

	private Point changeOrigin(Rect subImageRect, Point point) {
		return new Point(point.x - subImageRect.x, point.y - subImageRect.y);
	}

	private void drawPolygon(Mat image, Point[] polygon, Scalar color) {
		Point topLeft = polygon[0];
		Point topRight = polygon[1];
		Point bottomRight = polygon[2];
		Point bottomLeft = polygon[3];
		Imgproc.line(image, topLeft, topRight, color);
		Imgproc.line(image, topRight, bottomRight, color);
		Imgproc.line(image, bottomRight, bottomLeft, color);
		Imgproc.line(image, bottomLeft, topLeft, color);
	}

	private Point[] getPolygon(int i, int j) {
		return mesh.get(new Key(i, j));
	}

	private void addFirstPoly(Point imgCenter) {
		Point bottomRight = imgCenter;
		Point topRight = verticalMove(bottomRight, -deltaY);
		Point bottomLeft = horizontalMove(bottomRight, -deltaX);
		Point topLeft = intersect(topRight, bottomLeft);
		points.add(bottomRight);
		points.add(topRight);
		points.add(bottomLeft);
		points.add(topLeft);
		Point[] polygon = { topLeft, topRight, bottomRight, bottomLeft };
		mesh.put(new Key(0, 0), polygon);
	}

	private void addPolygon(int i, int j) { // ajoute un polygone compte tenu des polygones voisins
		// System.out.println(i + " " + j);
		// les polygones au dessus, à droite, en dessous et à gauche
		Point[] abovePolygon = getPolygon(i - 1, j);
		Point[] rightPolygon = getPolygon(i, j + 1);
		Point[] belowPolygon = getPolygon(i + 1, j);
		Point[] leftPolygon = getPolygon(i, j - 1);

		// les points du polygone à terminer
		Point topLeft, topRight, bottomRight, bottomLeft;

		if (abovePolygon != null) { // si le polygone du dessus existe
			topLeft = abovePolygon[3];
			topRight = abovePolygon[2];
			if (leftPolygon != null) { // si le polygone de gauche existe aussi
				bottomLeft = leftPolygon[2];
				bottomRight = intersect(bottomLeft, topRight);
				points.add(bottomRight);
			} else if (rightPolygon != null) { // sinon si le polygone de droite existe aussi
				bottomRight = rightPolygon[3];
				bottomLeft = intersect(bottomRight, topLeft);
				points.add(bottomLeft);
			} else { // s'il n'y a que le polygone du dessus
				bottomLeft = verticalMove(topLeft, deltaY);
				bottomRight = verticalMove(topRight, deltaY);
				points.add(bottomLeft);
				points.add(bottomRight);
			}
		} else if (rightPolygon != null) { // si le polygone de droite existe mais pas celui du dessus
			topRight = rightPolygon[0];
			bottomRight = rightPolygon[3];
			if (belowPolygon != null) { // si le polygone du dessous existe aussi
				bottomLeft = belowPolygon[0];
				topLeft = intersect(topRight, bottomLeft);
				points.add(topLeft);
			} else { // s'il n'y a que le polygone de droite
				topLeft = horizontalMove(topRight, -deltaX);
				bottomLeft = horizontalMove(bottomRight, -deltaX);
				points.add(topLeft);
				points.add(bottomLeft);
			}
		} else if (belowPolygon != null) { // si le polygone du dessous existe
			bottomLeft = belowPolygon[0];
			bottomRight = belowPolygon[1];
			if (leftPolygon != null) { // si le polygone de gauche existe aussi
				topLeft = leftPolygon[1];
				topRight = intersect(topLeft, bottomRight);
				points.add(topRight);
			} else { // s'il n'y a que le polygone du dessous
				topLeft = verticalMove(bottomLeft, -deltaY);
				topRight = verticalMove(bottomRight, -deltaY);
				points.add(topLeft);
				points.add(topRight);
			}
		} else if (leftPolygon != null) { // s'il n'y a que le polygone de gauche
			topLeft = leftPolygon[1];
			bottomLeft = leftPolygon[2];
			topRight = horizontalMove(topLeft, deltaX);
			bottomRight = horizontalMove(bottomLeft, deltaX);
			points.add(topRight);
			points.add(bottomRight);
		} else { // s'il n'y a aucun autre polygone, c'est le premier
			throw new IllegalStateException();
			// bottomRight = addPoint(imgCenter);
			// topRight = verticalPrevious(bottomRight);
			// bottomLeft = horizontalPrevious(bottomRight);
			// topLeft = intersect(topRight, bottomLeft);
		}

		Point[] polygon = new Point[] { topLeft, topRight, bottomRight, bottomLeft };
		mesh.put(new Key(i, j), polygon);
	}

	private Point intersect(Point hPoint, Point vPoint) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint
		Point intersection = null;
		double xDiff, yDiff;
		for (int i = 0; i < 1000; i++) {
			xDiff = xDiff(hPoint, vPoint);
			yDiff = yDiff(vPoint, hPoint);
			hPoint = horizontalMove(hPoint, xDiff);
			vPoint = verticalMove(vPoint, yDiff);
			if (Math.abs(xDiff) < 0.5 && Math.abs(yDiff) < 0.5) {
				intersection = new Point(0.5 * (hPoint.x + vPoint.x), 0.5 * (hPoint.y + vPoint.y));
				return intersection;
			}
		}
		throw new IllegalStateException();
	}

	private double xDiff(Point pt1, Point pt2) {
		return pt2.x - pt1.x;
	}

	private double yDiff(Point pt1, Point pt2) {
		return pt2.y - pt1.y;
	}

	private Point verticalMove(Point startingPoint, double deltaY) {
		double dY = deltaY / nbIter; // proche de 1 pixel
		double x = startingPoint.x, y = startingPoint.y;
		for (int i = 0; i < nbIter; i++) {
			double[] angles = interpolator.interpolate(x, y);
			double dX = dY / Math.tan(angles[1]);
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	private Point horizontalMove(Point startingPoint, double deltaX) {
		double dX = deltaX / nbIter; // proche de 1 pixel
		double x = startingPoint.x, y = startingPoint.y;
		for (int i = 0; i < nbIter; i++) {
			double[] angles = interpolator.interpolate(x, y);
			double dY = Math.tan(angles[0]) * dX;
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	private class Key { // tableau multidimensionnel

		public int i, j; // i = numero de ligne, j = numero de colonne

		public Key(int i, int j) {
			this.i = i;
			this.j = j;
		}

		@Override
		public int hashCode() {
			return 31 * i + 7 * j;
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof Key))
				return false;
			Key other = (Key) obj;
			return i == other.i && j == other.j;
		}
	}
}
