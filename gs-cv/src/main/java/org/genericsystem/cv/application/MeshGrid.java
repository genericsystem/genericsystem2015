package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MeshGrid {

	private static final Logger logger = LoggerFactory.getLogger(MeshGrid.class);
	private Size kSize;
	protected List<Point> points = new ArrayList<>();
	// Array of points: top left, top right, bottom right, bottom left.
	protected Map<Key, Point[]> mesh = new HashMap<>(); // les mêmes polygones mais en i, j
	private Interpolator interpolator;
	public double deltaX, deltaY; // déplacement d'un polygone
	protected int xBorder;
	protected int yBorder;
	protected Mat image;

	private int nbIter; // nombre d'itérations à chaque déplacement

	public MeshGrid(Mat image, int xBorder, int yBorder) {
		this.xBorder = xBorder;
		this.yBorder = yBorder;
		this.image = new Mat();
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_REPLICATE);
	}

	public MeshGrid(Size kSize, Interpolator interpolator, double deltaX, double deltaY, Mat image) {
		this.kSize = kSize;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		this.image = new Mat();
		xBorder = 2 * (int) deltaX;
		yBorder = 2 * (int) deltaY;
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_REPLICATE);
		nbIter = (int) Math.round(deltaY); // avance d'un pixel à chaque itération
	}

	private Size getOldSize() {
		return new Size(image.width() - 2 * xBorder, image.height() - 2 * yBorder);
	}

	public void build() {
		Point imgCenter = new Point(image.width() / 2, image.height() / 2);
		addFirstPoly(imgCenter);
		for (int i = 0; i <= kSize.height; i++) {
			for (int j = 0; j <= (int) kSize.width; j++)
				if (i != 0 || j != 0)
					addPolygon(i, j);
			for (int j = -1; j > -(int) kSize.width; j--)
				addPolygon(i, j);
		}
		for (int i = -1; i > -kSize.height; i--) {
			for (int j = 0; j <= (int) kSize.width; j++)
				addPolygon(i, j);
			for (int j = -1; j > -(int) kSize.width; j--)
				addPolygon(i, j);
		}
	}

	public Mat drawOnCopy(Scalar color) {
		Mat clone = image.clone();
		mesh.values().forEach(p -> drawPolygon(clone, p, color));
		return clone;
	}

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

	public Mat dewarp() {
		int rectHeight = (int) Math.floor(getOldSize().height / (2 * kSize.height));
		int rectWidth = (int) Math.floor(getOldSize().width / (2 * kSize.width));

		Mat dewarpedImage = new Mat(getOldSize(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int i = (int) -kSize.height + 1; i <= kSize.height; i++) {
			for (int j = (int) -kSize.width + 1; j <= kSize.width; j++) {
				if (inImageBorders(mesh.get(new Key(i, j)))) {
					Rect subImageRect = subImageRect(i, j);
					Mat homography = dewarpPolygon(mesh.get(new Key(i, j)), subImageRect, rectHeight, rectWidth);
					int x = (j + (int) kSize.width - 1) * rectWidth;
					int y = (i + (int) kSize.height - 1) * rectHeight;
					assert x >= 0 && y >= 0 && ((x + rectWidth) <= dewarpedImage.width()) && ((y + rectHeight) <= dewarpedImage.height()) : "x: " + x + ", y: " + y + ", width: " + image.width() + ", height: " + image.height() + " , rectWidth : "
							+ rectWidth + " , rectHeight : " + rectHeight;
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

	protected Rect subImageRect(int iP, int jP) {
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
				assert j == 0;
				if (i > 0) {
					bottomLeft = verticalMove(topLeft, deltaY);
					bottomRight = intersect(bottomLeft, topRight);
				} else {
					bottomRight = verticalMove(topRight, deltaY);
					bottomLeft = intersect(topLeft, bottomRight);
				}
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
				assert i == 0;
				if (j > 0) {
					topLeft = horizontalMove(topRight, -deltaX);
					bottomLeft = intersect(bottomRight, topLeft);
				} else {
					bottomLeft = horizontalMove(bottomRight, -deltaX);
					topLeft = intersect(topRight, bottomLeft);
				}
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
				assert j == 0;
				if (i > 0) {
					topLeft = verticalMove(bottomLeft, -deltaY);
					topRight = intersect(bottomRight, topLeft);
				} else {
					topRight = verticalMove(bottomRight, -deltaY);
					topLeft = intersect(topRight, bottomLeft);
				}
				points.add(topLeft);
				points.add(topRight);
			}
		} else if (leftPolygon != null) { // s'il n'y a que le polygone de gauche
			assert i == 0;
			topLeft = leftPolygon[1];
			bottomLeft = leftPolygon[2];
			if (j > 0) {
				topRight = horizontalMove(topLeft, deltaX);
				bottomRight = intersect(bottomLeft, topRight);
			} else {
				bottomRight = horizontalMove(bottomLeft, deltaX);
				topRight = intersect(topLeft, bottomRight);
			}
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
		double dY = deltaY / nbIter;
		double x = startingPoint.x, y = startingPoint.y;
		for (int i = 0; i < nbIter; i++) {
			double dX = dY / Math.tan(interpolator.interpolateVerticals(x - xBorder, y - yBorder));
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	private Point horizontalMove(Point startingPoint, double deltaX) {
		double dX = deltaX / nbIter;
		double x = startingPoint.x, y = startingPoint.y;
		for (int i = 0; i < nbIter; i++) {
			double dY = Math.tan(interpolator.interpolateHorizontals(x - xBorder, y - yBorder)) * dX;
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}

	protected static class Key { // tableau multidimensionnel

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
