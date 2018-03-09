package org.genericsystem.cv.application;

import java.util.Arrays;

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

	double thetaMin = 0;
	double thetaMax = Math.PI;
	
	Scalar red = new Scalar(0, 0, 255, 0);
	Scalar green = new Scalar(0, 255, 0, 0);
	Scalar blue = new Scalar(255, 0, 0, 0);
	Size imgSize = new Size(800, 600);
	
	WeightedOrientedPointsInterpolator interpolator;
	Mat image;
	
	public static void main(String[] args) {
		launch(args);
	}
	
	private void drawSimpleLine(Mat image, int x, int y, double theta, Scalar color, double radius) {
		Point pt1 = new Point((int) Math.round(x+radius*Math.cos(theta)), (int) Math.round(y+radius*Math.sin(theta)));
    	Point pt2 = new Point((int) Math.round(x-radius*Math.cos(theta)), (int) Math.round(y-radius*Math.sin(theta)));
    	Imgproc.line(image, pt1, pt2, color);
	}

	private void drawFieldLine(double x0, double y0, double deltaY) {
		double dY = 1;
		double ratio = deltaY / dY;
		double x = x0, y = y0;
		Point pt1 = new Point(Math.round(x), Math.round(y));
		while(x > 0 && x < imgSize.width && y > 0 && y < imgSize.height) {
			double theta = interpolator.interpolate(x, y);
			double dX = dY/Math.tan(theta);
			x += dX;
			y += dY;
			if(y >= pt1.y + ratio * dY) {
				Point pt2 = new Point(Math.round(x), Math.round(y));
				Imgproc.line(image, pt1, pt2, green);
				pt1 = pt2;
			}
		}
	}
	
	private double angle(double p) { // p entre 0 et 1
		return thetaMin + p * (thetaMax - thetaMin);
	}
	
	private int grayScale(double theta) { // retourne une valeur entre 0 et 255
		return (int) Math.round(255 * (theta - thetaMin) / (thetaMax - thetaMin)) ;
	}
	
	static {
		NativeLibraryLoader.load();
	}
	
	@Override
	protected void fillGrid(GridPane mainGrid) {
		
		image = new Mat(new Size(800, 600), CvType.CV_8UC3);
		
		double [] [] weightedOrientedPoints = {
				{220, 100, angle(0.1), 1},
				{500, 400, angle(0.2), 1},
				{400, 200, angle(0.5), 1},
				{200, 400, angle(0.8), 1},
				{500, 140, angle(0.3), 1},
				{100, 260, angle(0.7), 1}
			};
		
		interpolator = new WeightedOrientedPointsInterpolator(weightedOrientedPoints, 0.5);
		
		for(int x = 0; x < imgSize.width; x++) {
			for(int y = 0; y < imgSize.height; y++) {
				double theta = interpolator.interpolate(x, y);
				int thetaGrayScale = grayScale(theta);
				image.put(y, x, thetaGrayScale, thetaGrayScale, thetaGrayScale);
			}
		}

		for(int x = 100; x < imgSize.width-1; x += 100) {
			for(int y = 100; y < imgSize.height-1; y += 100) {
				double theta = interpolator.interpolate(x, y);
	            drawSimpleLine(image, x, y, theta, blue, 20);
	            drawSimpleLine(image, x, y, theta+Math.PI/2, blue, 5);
			}
		}
		
		Arrays.stream(weightedOrientedPoints).forEach(p -> {
			drawSimpleLine(image, (int) p[0], (int) p[1], p[2], red, 20);
			drawSimpleLine(image, (int) p[0], (int) p[1], p[2]+Math.PI/2, red, 5);
		});
		
		for(int x0 = 20 ; x0 < 780 ; x0 += 20 ) {
			drawFieldLine(x0, 10, 1);
		}
		
		Img img = new Img(image);
		mainGrid.add(new ImageView(img.toJfxImage()), 0, 0);
	}

}
