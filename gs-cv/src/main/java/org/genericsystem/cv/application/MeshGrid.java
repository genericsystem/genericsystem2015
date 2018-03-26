package org.genericsystem.cv.application;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MeshGrid {

	public int size; // nombre de polygones par côté
	public List<Point> points = new ArrayList<>();
	private Map<Key, Integer[]> mesh = new HashMap<>(); // les mêmes polygones mais en i, j
	SuperContourInterpolator interpolator;
	public double deltaX, deltaY; // déplacement d'un polygone

	private int nbIter; // nombre d'itérations à chaque déplacement
	// private int iP = 0, jP = 0; // polygone courant
	private int minIndex, maxIndex; // indices min et max des polygones
	private double rectWidth, rectHeight; // taille d'un polygone après redressement

	public MeshGrid(int size, SuperContourInterpolator interpolator, double deltaX, double deltaY) {
		this.size = size;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
	}

	public void build(Point imgCenter) {
		nbIter = (int) Math.round(deltaY); // avance d'un pixel à chaque itération
		int iP = 0;
		int jP = 0;
		addFirstPoly(imgCenter);
		for (int k = 1; k < size; k++) {
			int dir = k % 2 == 0 ? -1 : 1;
			for (int l = 1; l <= k; l++) {
				jP += dir;
				addPolygon(iP, jP);
			}
			for (int l = 1; l <= k; l++) {
				iP += dir;
				addPolygon(iP, jP);
			}
		}
		for (int l = 1; l < size; l++) {
			jP += size % 2 == 0 ? -1 : 1;
			addPolygon(iP, jP);
		}
	}

	public void draw(Mat img, Scalar color) {
		mesh.values().forEach(p -> drawPolygon(img, p, color));
	}

	public Mat dewarp(Mat image, Size dewarpedSize) {

		minIndex = -size / 2 + 1;
		maxIndex = size / 2;
		rectHeight = dewarpedSize.height / size;
		// rectWidth = dewarpedSize.width / size;
		Mat dewarpedImage = new Mat(dewarpedSize, CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int iP = minIndex; iP <= maxIndex; iP++) {
			for (int jP = minIndex; jP <= maxIndex; jP++) {
				Rect subImageRect = subImageRect(iP, jP);
				double xInf = subImageRect.x, yInf = subImageRect.y;
				if (xInf > 0 && yInf > 0 && xInf + subImageRect.width < image.width() && yInf + subImageRect.height < image.height()) {
					Mat homography = dewarpPolygon(mesh.get(new Key(iP, jP)), subImageRect);
					double x = (int) Math.floor(dewarpedSize.width / 2) + (jP - 1) * rectHeight;
					double y = (int) Math.floor(dewarpedSize.height / 2) + (iP - 1) * rectHeight;
					Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectHeight, y + rectHeight)));
					Mat subImage = new Mat(image, subImageRect);
					Imgproc.warpPerspective(subImage, subDewarpedImage, homography, new Size(rectHeight, rectHeight), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
				}
			}
		}
		return dewarpedImage;

	}

	private Rect subImageRect(int iP, int jP) {

		Integer[] polygon = mesh.get(new Key(iP, jP));
		Point warpedTopLeft = points.get(polygon[0]);
		Point warpedTopRight = points.get(polygon[1]);
		Point warpedBottomRight = points.get(polygon[2]);
		Point warpedBottomLeft = points.get(polygon[3]);
		double xMin, xMax, yMin, yMax;
		xMin = Math.min(warpedTopLeft.x, warpedBottomLeft.x);
		xMax = Math.max(warpedBottomRight.x, warpedTopRight.x);
		yMin = Math.min(warpedTopRight.y, warpedTopLeft.y);
		yMax = Math.max(warpedBottomLeft.y, warpedBottomRight.y);
		return new Rect(new Point(xMin, yMin), new Point(xMax, yMax));

	}

	private Mat dewarpPolygon(Integer[] polygon, Rect subImageRect) {

		Point warpedTopLeft = changeOrigin(subImageRect, points.get(polygon[0]));
		Point warpedTopRight = changeOrigin(subImageRect, points.get(polygon[1]));
		Point warpedBottomRight = changeOrigin(subImageRect, points.get(polygon[2]));
		Point warpedBottomLeft = changeOrigin(subImageRect, points.get(polygon[3]));
		Point dewarpedTopLeft = new Point(0, 0);
		Point dewarpedTopRight = new Point(rectHeight, 0);
		Point dewarpedBottomRight = new Point(rectHeight, rectHeight);
		Point dewarpedBottomLeft = new Point(0, rectHeight);
		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(warpedTopLeft, warpedTopRight, warpedBottomRight, warpedBottomLeft), new MatOfPoint2f(dewarpedTopLeft, dewarpedTopRight, dewarpedBottomRight, dewarpedBottomLeft));

	}

	private Point changeOrigin(Rect subImageRect, Point point) {
		return new Point(point.x - subImageRect.x, point.y - subImageRect.y);
	}

	private void drawPolygon(Mat image, Integer[] polygon, Scalar color) {
		Point topLeft = points.get(polygon[0]);
		Point topRight = points.get(polygon[1]);
		Point bottomRight = points.get(polygon[2]);
		Point bottomLeft = points.get(polygon[3]);
		Imgproc.line(image, topLeft, topRight, color);
		Imgproc.line(image, topRight, bottomRight, color);
		Imgproc.line(image, bottomRight, bottomLeft, color);
		Imgproc.line(image, bottomLeft, topLeft, color);
	}

	private Integer[] getPolygon(int i, int j) {
		return mesh.get(new Key(i, j));
	}

	private void addFirstPoly(Point imgCenter) {
		int bottomRight = addPoint(imgCenter);
		int topRight = addPoint(verticalMove(points.get(bottomRight), -deltaY));
		int bottomLeft = addPoint(horizontalMove(points.get(bottomRight), -deltaX));
		int topLeft = intersect(topRight, bottomLeft);
		Integer[] polygon = { topLeft, topRight, bottomRight, bottomLeft };
		mesh.put(new Key(0, 0), polygon);
	}

	private void addPolygon(int i, int j) { // ajoute un polygone compte tenu des polygones voisins

		// les polygones au dessus, à droite, en dessous et à gauche
		Integer[] abovePolygon = getPolygon(i - 1, j);
		Integer[] rightPolygon = getPolygon(i, j + 1);
		Integer[] belowPolygon = getPolygon(i + 1, j);
		Integer[] leftPolygon = getPolygon(i, j - 1);

		// les points du polygone à terminer
		int topLeft, topRight, bottomRight, bottomLeft;

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
				bottomLeft = verticalNext(topLeft);
				bottomRight = verticalNext(topRight);
			}
		} else if (rightPolygon != null) { // si le polygone de droite existe mais pas celui du dessus
			topRight = rightPolygon[0];
			bottomRight = rightPolygon[3];
			if (belowPolygon != null) { // si le polygone du dessous existe aussi
				bottomLeft = belowPolygon[0];
				topLeft = intersect(topRight, bottomLeft);
			} else { // s'il n'y a que le polygone de droite
				topLeft = horizontalPrevious(topRight);
				bottomLeft = horizontalPrevious(bottomRight);
			}
		} else if (belowPolygon != null) { // si le polygone du dessous existe
			bottomLeft = belowPolygon[0];
			bottomRight = belowPolygon[1];
			if (leftPolygon != null) { // si le polygone de gauche existe aussi
				topLeft = leftPolygon[1];
				topRight = intersect(topLeft, bottomRight);
			} else { // s'il n'y a que le polygone du dessous
				topLeft = verticalPrevious(bottomLeft);
				topRight = verticalPrevious(bottomRight);
			}
		} else if (leftPolygon != null) { // s'il n'y a que le polygone de gauche
			topLeft = leftPolygon[1];
			bottomLeft = leftPolygon[2];
			topRight = horizontalNext(topLeft);
			bottomRight = horizontalNext(bottomLeft);
		} else { // s'il n'y a aucun autre polygone, c'est le premier
			throw new IllegalStateException();
			// bottomRight = addPoint(imgCenter);
			// topRight = verticalPrevious(bottomRight);
			// bottomLeft = horizontalPrevious(bottomRight);
			// topLeft = intersect(topRight, bottomLeft);
		}

		Integer[] polygon = new Integer[] { topLeft, topRight, bottomRight, bottomLeft };
		mesh.put(new Key(i, j), polygon);

	}

	private int intersect(int hPt, int vPt) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint
		Point hPoint = points.get(hPt);
		Point vPoint = points.get(vPt);
		Point intersection = null;
		double xDiff = xDiff(hPoint, vPoint);
		double yDiff = yDiff(vPoint, hPoint);
		for (int i = 1; i < 1000; i++) {
			xDiff = xDiff(hPoint, vPoint);
			yDiff = yDiff(vPoint, hPoint);
			hPoint = horizontalMove(hPoint, xDiff);
			vPoint = verticalMove(vPoint, yDiff);
			if (Math.abs(xDiff) < 0.5 && Math.abs(yDiff) < 0.5) {
				intersection = new Point(0.5 * (hPoint.x + vPoint.x), 0.5 * (hPoint.y + vPoint.y));
				return addPoint(intersection);
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

	private int verticalNext(int point) {
		return addPoint(verticalMove(points.get(point), deltaY));
	}

	private int verticalPrevious(int point) {
		return addPoint(verticalMove(points.get(point), -deltaY));
	}

	private int horizontalNext(int point) {
		return addPoint(horizontalMove(points.get(point), deltaX));
	}

	private int horizontalPrevious(int point) {
		return addPoint(horizontalMove(points.get(point), -deltaX));
	}

	private int addPoint(Point point) { // ajoute un point et renvoie son index
		points.add(point);
		return points.size() - 1;
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
