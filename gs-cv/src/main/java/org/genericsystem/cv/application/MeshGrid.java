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

	
	public Mat image;
	public int size; // nombre de polygones par côté
	public List<Point> points = new ArrayList<>();
	public List<Integer[]> polygons = new ArrayList<>(); // les polygones dans une liste
	private Map<Key, Integer[]> mesh = new HashMap<>(); // les mêmes polygones mais en i, j
	SuperContourInterpolator interpolator;
	public double deltaX, deltaY; // déplacement d'un polygone
	
	private final int nbIter = 40; // nombre d'itérations à chaque déplacement
	private int iP = 0, jP = 0; // polygone courant
	private int minIndex, maxIndex; // indices min et max des polygones
	private double rectWidth, rectHeight; // taille d'un polygone après redressement
	
	public MeshGrid(int size, Mat image, SuperContourInterpolator interpolator, double deltaX, double deltaY) {
		this.size = size;
		this.image = image;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
	}

	public void build() {
		addPolygon(iP, jP);
		for(int k = 1 ; k < size ; k++) {
			int dir = k%2 == 0 ? -1 : 1;
			horizontalSpiral(k, dir);
			verticalSpiral(k, dir);
		}
		horizontalSpiral(size - 1, size%2 == 0 ? -1 : 1);
	}
		
	public void draw(Scalar color) {
		polygons.forEach(p -> drawPolygon(p, color));
	}
	
	public Mat dewarp(int dewarpedImageSize) {
		
		minIndex = (int) -Math.floor((size - 1)/2);
		maxIndex = (int) Math.floor(size/2);
		rectWidth = dewarpedImageSize/size;
		rectHeight = dewarpedImageSize/size;
		Mat dewarpedImage = new Mat(new Size(dewarpedImageSize, dewarpedImageSize), CvType.CV_8UC3);
		for(int iP = minIndex ; iP <= maxIndex ; iP++) {
			for(int jP = minIndex ; jP <= maxIndex ; jP++) {
				Mat homography = dewarpPolygon(iP, jP);
				double x = (jP - minIndex) * rectWidth;
				double y = (iP - minIndex) * rectHeight;
				Point dewarpedTopLeft = new Point(x, y);
				Point dewarpedBottomRight = new Point(x + rectWidth, y + rectHeight);
				Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(dewarpedTopLeft, dewarpedBottomRight));
				Mat subImage = subImage(iP, jP);
				Imgproc.warpPerspective(subImage, subDewarpedImage, homography, new Size(rectWidth, rectHeight), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
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
	
	private Mat subImage(int iP, int jP){
		
		Rect subImageRect = subImageRect(iP, jP);
    	//Imgproc.rectangle(image, new Point(subImageRect.x, subImageRect.y), new Point(subImageRect.x+subImageRect.width, subImageRect.y+subImageRect.height), new Scalar(255, 0, 0));
		return new Mat(image, subImageRect);
		
	}
	
	private Mat dewarpPolygon(int iP, int iJ) {
		
		Rect subImageRect = subImageRect(iP, jP);
		Integer[] polygon = mesh.get(new Key(iP, jP));
		Point warpedTopLeft = changeOrigin(subImageRect, points.get(polygon[0]));
		Point warpedTopRight = changeOrigin(subImageRect, points.get(polygon[1]));
		Point warpedBottomRight = changeOrigin(subImageRect, points.get(polygon[2]));
		Point warpedBottomLeft = changeOrigin(subImageRect, points.get(polygon[3]));	
		Point dewarpedTopLeft = new Point(0, 0);
		Point dewarpedTopRight = new Point(rectWidth, 0);
		Point dewarpedBottomRight = new Point(rectWidth, rectHeight);
		Point dewarpedBottomLeft = new Point(0, rectHeight);
		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(warpedTopLeft, warpedTopRight, warpedBottomRight, warpedBottomLeft), 
				new MatOfPoint2f(dewarpedTopLeft, dewarpedTopRight, dewarpedBottomRight, dewarpedBottomLeft));
	
	}
	
	private Point changeOrigin(Rect subImageRect, Point point) {
		return new Point(point.x-subImageRect.x, point.y-subImageRect.y);
	}
	
	private void drawPolygon(Integer[] polygon, Scalar color) {
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
	
	private void horizontalSpiral(int n, int dir) { // avance horizontale de n polygones dans la spirale, vers la droite (dir = +1) ou la gauche (dir = -1)
		for(int k = 1; k <= n; k++) {
			jP += dir;
			addPolygon(iP, jP);
		}
	}
	
	private void verticalSpiral(int n, int dir) { // avance verticale de n polygones dans la spirale, vers le haut (dir = -1) ou le bas (dir = +1);
		for(int k = 1; k <= n; k++) {
			iP += dir;
			addPolygon(iP, jP);
		}
	}
	
	private void addPolygon(int i, int j) { // ajoute un polygone compte tenu des polygones voisins
		
		// les polygones au dessus, à droite, en dessous et à gauche
		Integer[] abovePolygon = getPolygon(i - 1, j);
		Integer[] rightPolygon = getPolygon(i, j + 1); 
		Integer[] belowPolygon = getPolygon(i + 1, j);
		Integer[] leftPolygon = getPolygon(i, j - 1); 
		
		// les points du polygone à terminer
		int topLeft, topRight, bottomRight, bottomLeft;
		
		if(abovePolygon != null) { // si le polygone du dessus existe
			topLeft = abovePolygon[3];
			topRight = abovePolygon[2];
			if(leftPolygon != null) { // si le polygone de gauche existe aussi
				bottomLeft = leftPolygon[2];
				bottomRight = intersect(bottomLeft, topRight);
			} else if (rightPolygon != null) { // sinon si le polygone de droite existe aussi
				bottomRight = rightPolygon[3];
				bottomLeft = intersect(bottomRight, topLeft);
			} else { // s'il n'y a que le polygone du dessus
				bottomLeft = verticalNext(topLeft);
				bottomRight = verticalNext(topRight);
			}
		} else if(rightPolygon != null) { // si le polygone de droite existe mais pas celui du dessus
			topRight = rightPolygon[0];
			bottomRight = rightPolygon[3];
			if(belowPolygon != null) { // si le polygone du dessous existe aussi
				bottomLeft = belowPolygon[0];
				topLeft = intersect(topRight, bottomLeft);
			} else { // s'il n'y a que le polygone de droite
				topLeft = horizontalPrevious(topRight);
				bottomLeft = horizontalPrevious(bottomRight); 
			}
		} else if(belowPolygon != null) { // si le polygone du dessous existe
			bottomLeft = belowPolygon[0];
			bottomRight = belowPolygon[1];
			if(leftPolygon != null) { // si le polygone de gauche existe aussi
				topLeft = leftPolygon[1];
				topRight = intersect(topLeft, bottomRight);
			} else { // s'il n'y a que le polygone du dessous
				topLeft = verticalPrevious(bottomLeft);
				topRight = verticalPrevious(bottomRight);
			}
		} else if(leftPolygon != null) { // s'il n'y a que le polygone de gauche
			topLeft = leftPolygon[1];
			bottomLeft = leftPolygon[2];
			topRight = horizontalNext(topLeft);
			bottomRight = horizontalNext(bottomLeft);
		} else { // s'il n'y a aucun autre polygone, c'est le premier
			bottomRight = addPoint(new Point(image.size().width/2, image.size().height/2));
			topRight = verticalPrevious(bottomRight);
			bottomLeft = horizontalPrevious(bottomRight);
			topLeft = intersect(topRight, bottomLeft);
		}
		
		Integer[] polygon = new Integer[] { topLeft, topRight, bottomRight, bottomLeft };
		mesh.put(new Key(i, j), polygon);
		polygons.add(polygon);
		
	}
	
	private int intersect(int hPt, int vPt) { // intersection de la ligne horizontale partant de hPoint avec la ligne verticale partant de vPoint 
		Point hPoint = points.get(hPt);
		Point vPoint = points.get(vPt);
		Point intersection = null;
		double xDiff = xDiff(hPoint, vPoint);
		double yDiff = yDiff(vPoint, hPoint);
		for(int i = 1; i < 1000 ; i++) {
			xDiff = xDiff(hPoint, vPoint);
			yDiff = yDiff(vPoint, hPoint);
			hPoint = horizontalMove(hPoint, xDiff);
			vPoint = verticalMove(vPoint, yDiff);
			if(Math.abs(xDiff) < 0.5 && Math.abs(yDiff) < 0.5) {
				intersection = new Point(0.5*(hPoint.x+vPoint.x), 0.5*(hPoint.y+vPoint.y));
				break;
			}
		}
		return addPoint(intersection);
	}
	
	private double xDiff(Point pt1, Point pt2) {
		return pt2.x-pt1.x;
	}
	
	private double yDiff(Point pt1, Point pt2) {
		return pt2.y-pt1.y;
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
		double dY = deltaY/nbIter;
		double x = startingPoint.x, y = startingPoint.y;
		for(int i = 0 ; i < nbIter ; i++) {
			double [] angles = interpolator.interpolate(x, y);
			double dX = dY/Math.tan(angles[1]);
			x += dX;
			y += dY;
		}
		return new Point(x, y);
	}
	
	private Point horizontalMove(Point startingPoint, double deltaX) {
		double dX = deltaX/nbIter;
		double x = startingPoint.x, y = startingPoint.y;
		for(int i = 0 ; i < nbIter ; i++) {
			double [] angles = interpolator.interpolate(x, y);
			double dY = Math.tan(angles[0])*dX;
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
			return 31*i + 7*j;
		}
		
		@Override
		public boolean equals(Object obj) {
			if(!(obj instanceof Key))
				return false;
			Key other = (Key) obj;
			return i == other.i && j == other.j;
		}
	
	}
	
}
