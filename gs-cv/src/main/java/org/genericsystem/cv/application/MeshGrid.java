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

public class MeshGrid {

	private final Size kSize;
	public List<Point> points = new ArrayList<>();
	private Map<Key, Point[]> mesh = new HashMap<>(); // les mêmes polygones mais en i, j
	SuperContourInterpolator interpolator;
	public double deltaX, deltaY; // déplacement d'un polygone

	private int nbIter; // nombre d'itérations à chaque déplacement

	public MeshGrid(Size kSize, SuperContourInterpolator interpolator, double deltaX, double deltaY) {
		this.kSize = kSize;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		nbIter = (int) Math.round(deltaY); // avance d'un pixel à chaque itération
	}

	public void build(Point imgCenter) {

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

	public Mat dewarp(Mat image) {

		int rectHeight = (int) Math.floor(image.height() / (2 * kSize.height));

		Mat dewarpedImage = new Mat(image.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int i = (int) -kSize.height; i <= kSize.height; i++) {
			for (int j = (int) -kSize.width; j <= kSize.width; j++) {
				Rect subImageRect = subImageRect(i, j);
				if (subImageRect.tl().x >= 0 && subImageRect.tl().y >= 0 && subImageRect.br().x < image.width() && subImageRect.br().y < image.height()) {
					Mat homography = dewarpPolygon(mesh.get(new Key(i, j)), subImageRect, rectHeight);
					double x = Math.floor(image.width() / 2) + (j - 1) * rectHeight;
					double y = Math.floor(image.height() / 2) + (i - 1) * rectHeight;
					if (x >= 0 && y >= 0 && ((x + rectHeight) <= image.width()) && ((y + rectHeight) <= image.height())) {
						Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectHeight, y + rectHeight)));
						Mat subImage = new Mat(image, subImageRect);
						Imgproc.warpPerspective(subImage, subDewarpedImage, homography, new Size(rectHeight, rectHeight), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
					}
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

	private Mat dewarpPolygon(Point[] polygon, Rect subImageRect, double rectHeight) {

		Point warpedTopLeft = changeOrigin(subImageRect, polygon[0]);
		Point warpedTopRight = changeOrigin(subImageRect, polygon[1]);
		Point warpedBottomRight = changeOrigin(subImageRect, polygon[2]);
		Point warpedBottomLeft = changeOrigin(subImageRect, polygon[3]);
		Point dewarpedTopLeft = new Point(0, 0);
		Point dewarpedTopRight = new Point(rectHeight, 0);
		Point dewarpedBottomRight = new Point(rectHeight, rectHeight);
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
			} else if (rightPolygon != null) { // sinon si le polygone de droite existe aussi
				bottomRight = rightPolygon[3];
				bottomLeft = intersect(bottomRight, topLeft);
			} else { // s'il n'y a que le polygone du dessus
				bottomLeft = verticalMove(topLeft, deltaY);
				bottomRight = verticalMove(topRight, deltaY);
			}
		} else if (rightPolygon != null) { // si le polygone de droite existe mais pas celui du dessus
			topRight = rightPolygon[0];
			bottomRight = rightPolygon[3];
			if (belowPolygon != null) { // si le polygone du dessous existe aussi
				bottomLeft = belowPolygon[0];
				topLeft = intersect(topRight, bottomLeft);
			} else { // s'il n'y a que le polygone de droite
				topLeft = horizontalMove(topRight, -deltaX);
				bottomLeft = horizontalMove(bottomRight, -deltaX);
			}
		} else if (belowPolygon != null) { // si le polygone du dessous existe
			bottomLeft = belowPolygon[0];
			bottomRight = belowPolygon[1];
			if (leftPolygon != null) { // si le polygone de gauche existe aussi
				topLeft = leftPolygon[1];
				topRight = intersect(topLeft, bottomRight);
			} else { // s'il n'y a que le polygone du dessous
				topLeft = verticalMove(bottomLeft, -deltaY);
				topRight = verticalMove(bottomRight, -deltaY);
			}
		} else if (leftPolygon != null) { // s'il n'y a que le polygone de gauche
			topLeft = leftPolygon[1];
			bottomLeft = leftPolygon[2];
			topRight = horizontalMove(topLeft, deltaX);
			bottomRight = horizontalMove(bottomLeft, deltaX);
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
