package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class WeightedOrientedPointsInterpolatorDemo extends AbstractApp {

	Scalar red = new Scalar(0, 0, 255, 0);
	Scalar green = new Scalar(0, 255, 0, 0);
	Scalar blue = new Scalar(255, 0, 0, 0);
	Size imgSize = new Size(800, 600);
	
	WeightedOrientedPointsInterpolator interpolator;
	Mat hAngleImage;
	Mat vAngleImage;
	
	Mat gridImage;
	
	public static void main(String[] args) {
		launch(args);
	}
	
	private void drawSimpleLine(Mat image, double x, double y, double theta, Scalar color, double radius) {
		Point pt1 = new Point(x+radius*Math.cos(theta), y+radius*Math.sin(theta));
    	Point pt2 = new Point(x-radius*Math.cos(theta), y-radius*Math.sin(theta));
    	Imgproc.line(image, pt1, pt2, color);
	}
	
	
	private Point verticalMove(Point startingPoint, double deltaY, int nbIter) {
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
	
	private Point horizontalMove(Point startingPoint, double deltaX, int nbIter) {
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
	
	private double xDistance(Point pt1, Point pt2) {
		return Math.abs(pt2.x-pt1.x);
	}
	
	private double yDistance(Point pt1, Point pt2) {
		return Math.abs(pt2.y-pt1.y);
	}
	
	private void drawPolygon(Point[] polygon, Scalar color) {
		System.out.println(polygon[3]);
		Imgproc.line(gridImage, polygon[0], polygon[1], color);
		Imgproc.line(gridImage, polygon[0], polygon[2], color);
		Imgproc.line(gridImage, polygon[1], polygon[3], color);
		Imgproc.line(gridImage, polygon[2], polygon[3], color);
	}
	
	private Point [] polygon(Point topLeftPoint) { // retourne le polygone qui part de topLeftPoint
		
		Point bottomLeftPoint = verticalMove(topLeftPoint, 50, 50);
		Point topRightPoint = horizontalMove(topLeftPoint, 50, 50);
		Point bottomRightPoint = null;
		Point pt1 = bottomLeftPoint;
		Point pt2 = topRightPoint;
		double deltaX = 5;
		double deltaY = 5;
		double previousXDistance = xDistance(pt1, pt2);
		double previousYDistance = yDistance(pt1, pt2);
		for(int i = 1; i < 2000 ; i++) {
			Point previousPt1 = pt1;
			Point previousPt2 = pt2;
			if(previousXDistance < 10) // on ralentit fort en avance pour éviter de dépasser le point
				deltaX = 1;
			if(previousXDistance >= 0.5)
				pt1 = horizontalMove(pt1, deltaX, 5);
			if(previousYDistance < 10) // on ralentit fort en avance pour éviter de dépasser le point
				deltaY = 1;
			if(previousYDistance >= 0.5)
				pt2 = verticalMove(pt2, deltaY, 5);
			if(previousXDistance < 0.5 && previousYDistance < 0.5) {
				bottomRightPoint = new Point(0.5*(previousPt1.x+previousPt2.x), 0.5*(previousPt1.y+previousPt2.y));
				break;
			}
			double xDistance = xDistance(pt1, pt2);
			double yDistance = yDistance(pt1, pt2);
			//System.out.println(xDistance+" ===== "+previousXDistance+" ===== "+yDistance+" ===== "+previousYDistance);
			if(previousXDistance < xDistance) { // si le point précédent était meilleur, on le garde
				pt1 = previousPt1;
				deltaX /= 2;
			} else
				previousXDistance = xDistance;
			if(previousYDistance < yDistance) { // si le point précédent était meilleur, on le garde
				pt2 = previousPt2;
				deltaY /= 2;
			} else
				previousYDistance = yDistance;
			
		}
		//Imgproc.line(gridImage, bottomLeftPoint, pt1, red);
		//Imgproc.line(gridImage, topRightPoint, pt2, red);
		return new Point [] {topLeftPoint, topRightPoint, bottomLeftPoint, bottomRightPoint};
		
	}
	
	private void drawVerticalFieldLine(double x0, double y0, double deltaY) {
		double dY = 1;
		double ratio = deltaY / dY;
		double x = x0, y = y0;
		Point pt1 = new Point(Math.round(x), Math.round(y));
		while(x > 0 && x < imgSize.width && y > 0 && y < imgSize.height) {
			double [] angles = interpolator.interpolate(x, y);
			double dX = dY/Math.tan(angles[1]);
			x += dX;
			y += dY;
			if(y >= pt1.y + ratio * dY) {
				Point pt2 = new Point(Math.round(x), Math.round(y));
				Imgproc.line(gridImage, pt1, pt2, green);
				pt1 = pt2;
			}
		}
	}
	
	private void drawHorizontalFieldLine(double x0, double y0, double deltaX) {
		double dX = 1;
		double ratio = deltaX / dX;
		double x = x0, y = y0;
		Point pt1 = new Point(Math.round(x), Math.round(y));
		while(x > 0 && x < imgSize.width && y > 0 && y < imgSize.height) {
			double angles [] = interpolator.interpolate(x, y);
			double dY = Math.tan(angles[0])*dX;
			x += dX;
			y += dY;
			if(x >= pt1.x + ratio * dX) {
				Point pt2 = new Point(Math.round(x), Math.round(y));
				Imgproc.line(gridImage, pt1, pt2, green);
				pt1 = pt2;
			}
		}
	}
	
	private double hAngle(double p) { // p entre 0 et 1 exclus
		return -Math.PI/2 + p * Math.PI;
	}
	
	private double vAngle(double p) { // p entre 0 et 1 exclus
		return p * Math.PI;
	}
	
	private int hAngleGrayScale(double theta) { // retourne une valeur entre 0 et 255
		return (int) Math.round(255 * (theta + Math.PI/2) / Math.PI) ;
	}
	
	private int vAngleGrayScale(double theta) { // retourne une valeur entre 0 et 255
		return (int) Math.round(255 * theta / Math.PI) ;
	}
	
	static {
		NativeLibraryLoader.load();
	}
	
	@Override
	protected void fillGrid(GridPane mainGrid) {
		
		hAngleImage = new Mat(imgSize, CvType.CV_8UC3);
		vAngleImage = new Mat(imgSize, CvType.CV_8UC3);
		gridImage = new Mat(imgSize, CvType.CV_8UC3);
		
		List<WeightedOrientedPoint> weightedOrientedPoints = new ArrayList<>();
		
		weightedOrientedPoints.add(new WeightedOrientedPoint(220, 100, hAngle(0.1), 1, vAngle(0.1), 1));
		weightedOrientedPoints.add(new WeightedOrientedPoint(500, 400, hAngle(0.2), 1, vAngle(0.2), 1));
		weightedOrientedPoints.add(new WeightedOrientedPoint(400, 200, hAngle(0.5), 1, vAngle(0.5), 1));
		weightedOrientedPoints.add(new WeightedOrientedPoint(200, 400, hAngle(0.8), 1, vAngle(0.8), 1));
		weightedOrientedPoints.add(new WeightedOrientedPoint(500, 140, hAngle(0.3), 1, vAngle(0.3), 1));
		weightedOrientedPoints.add(new WeightedOrientedPoint(100, 260, hAngle(0.7), 1, vAngle(0.7), 1));
		
		interpolator = new WeightedOrientedPointsInterpolator(weightedOrientedPoints, 0.5);
		
		for(int x = 0; x < imgSize.width; x++) {
			for(int y = 0; y < imgSize.height; y++) {
				double [] angles = interpolator.interpolate(x, y);
				int hAngleGrayScale = hAngleGrayScale(angles[0]);
				hAngleImage.put(y, x, hAngleGrayScale, hAngleGrayScale, hAngleGrayScale);
				int vAngleGrayScale = vAngleGrayScale(angles[1]);
				vAngleImage.put(y, x, vAngleGrayScale, vAngleGrayScale, vAngleGrayScale);
			}
		}

		for(int x = 100; x < imgSize.width-1; x += 100) {
			for(int y = 100; y < imgSize.height-1; y += 100) {
				double [] angles = interpolator.interpolate(x, y);
	            drawSimpleLine(hAngleImage, x, y, angles[0], blue, 20);
	            drawSimpleLine(hAngleImage, x, y, angles[0]+Math.PI/2, blue, 5);
	            drawSimpleLine(vAngleImage, x, y, angles[1], blue, 20);
	            drawSimpleLine(vAngleImage, x, y, angles[1]+Math.PI/2, blue, 5);
			}
		}
		
		weightedOrientedPoints.forEach(p -> {
			drawSimpleLine(hAngleImage, p.x, p.y, p.hAngle, red, 20);
			drawSimpleLine(hAngleImage, p.x, p.y, p.hAngle+Math.PI/2, red, 5);
			drawSimpleLine(vAngleImage, p.x, p.y, p.vAngle, red, 20);
			drawSimpleLine(vAngleImage, p.x, p.y, p.vAngle+Math.PI/2, red, 5);
		});
		
		
		for(int x0 = 20 ; x0 < 780 ; x0 += 40 ) {
			drawVerticalFieldLine(x0, 10, 1);
		}
		
		for(int y0 = 20 ; y0 < 580 ; y0 += 40 ) {
			drawHorizontalFieldLine(10, y0, 20);
		}
		
		drawPolygon(polygon(new Point(210, 210)), red);
		drawPolygon(polygon(new Point(300, 200)), red);
		drawPolygon(polygon(new Point(300, 300)), red);
		// drawPolygon(polygon(new Point(400, 400)), red); // ne fonctionne pas (va trop loin)
	    drawPolygon(polygon(new Point(400, 200)), red);
		
		Img hAngleImg = new Img(hAngleImage);
		mainGrid.add(new ImageView(hAngleImg.toJfxImage()), 0, 0);
		
		Img vAngleImg = new Img(vAngleImage);
		mainGrid.add(new ImageView(vAngleImg.toJfxImage()), 1, 0);
		
		Img gridImg = new Img(gridImage);
		mainGrid.add(new ImageView(gridImg.toJfxImage()), 0, 1);
	}

}
