package org.genericsystem.cv.application;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Rect;
import org.opencv.imgproc.Imgproc;

public class SuperContour implements Comparable<SuperContour> {

	public final MatOfPoint contour;
	public SuperContour succ;
	public SuperContour pred;
	public final Point center;
	public final double angle;
	public final double antiAngle;
	public final Point left;
	public final Point right;
	public final Point top;
	public final Point bottom;
	public final Rect rect;
	public final double lxmin, lxmax;
	public final double lymin, lymax;
	public final double dx, dy;
	public final boolean isLeaf;
	public double vertical;
	public Point vTop;
	public Point vBottom;

	public SuperContour(MatOfPoint contour, boolean isLeaf) {

		this.contour = contour;
		this.rect = Imgproc.boundingRect(contour);
		Mat dataPts = convertContourToMat(contour);
		Mat mean = new Mat();
		Mat eigen = new Mat();
		Core.PCACompute(dataPts, mean, eigen);
		Mat project = new Mat();
		Core.PCAProject(dataPts, mean, eigen, project);
		Mat min = new Mat();
		Core.reduce(project, min, 0, Core.REDUCE_MIN);
		Mat max = new Mat();
		Core.reduce(project, max, 0, Core.REDUCE_MAX);

		this.center = new Point(mean.get(0, 0)[0], mean.get(0, 1)[0]);

		this.lxmin = min.get(0, 0)[0];
		this.lymin = min.get(0, 1)[0];
		this.lxmax = max.get(0, 0)[0];
		this.lymax = max.get(0, 1)[0];

		Point tangent = new Point(eigen.get(0, 0)[0], eigen.get(0, 1)[0]);
		Point antiTangent = new Point(-eigen.get(0, 1)[0], eigen.get(0, 0)[0]);

		this.left = new Point(center.x + tangent.x * lxmin, center.y + tangent.y * lxmin);
		this.right = new Point(center.x + tangent.x * lxmax, center.y + tangent.y * lxmax);
		this.top = new Point(center.x + antiTangent.x * lymin, center.y + antiTangent.y * lymin);
		this.bottom = new Point(center.x + antiTangent.x * lymax, center.y + antiTangent.y * lymax);

		this.angle = Math.atan2(tangent.y, tangent.x);
		this.antiAngle = Math.atan2(antiTangent.y, antiTangent.x);

		this.isLeaf = isLeaf;
		this.dx = lxmax - lxmin;
		this.dy = lymax - lymin;
	}

	private int computeDistance(int ind, int other, int nBin) {
		return Math.min(Math.min(Math.abs(ind - other), Math.abs(ind - other - nBin)), Math.abs(ind - other + nBin));
	}

	int computeHisto(Mat mag, int[][] bin, int nBin, DirectionalFilter df, double squareSize) {
		double tlx = center.x - squareSize / 2;
		double tly = center.y - squareSize / 2;
		double brx = center.x + squareSize / 2;
		double bry = center.y + squareSize / 2;

		tlx = tlx > 0 ? tlx : 0;
		tly = tly > 0 ? tly : 0;

		brx = brx < mag.width() ? brx : mag.width();
		bry = bry < mag.height() ? bry : mag.height();

		Range rangey = new Range((int) tly, (int) bry);
		Range rangex = new Range((int) tlx, (int) brx);

		double[] histos = df.getHistogram(new Mat(mag, rangey, rangex), df.subArray(bin, rangey, rangex), nBin);
		int targetAngle = (int) (antiAngle / Math.PI * 64);
		// System.out.println("Angle " + angle / Math.PI * 180 + " Antiangle : " + antiAngle / Math.PI * 180);
		double max = Double.MIN_VALUE;
		int k = -1;
		// System.out.println(targetAngle);
		for (int i = 0; i < histos.length; i++) {

			if (computeDistance(i, targetAngle, nBin) < 12) {
				// System.out.println(i);
				if (histos[i] > max) {
					max = histos[i];
					k = i;
				}
			}
		}
		vertical = (Integer.valueOf(k).doubleValue() / 64) * Math.PI;
		// if (vertical > Math.PI)
		// vertical = vertical - Math.PI;
		vTop = new Point(center.x - dy * Math.cos(vertical), center.y - dy * Math.sin(vertical));
		vBottom = new Point(center.x + dy * Math.cos(vertical), center.y + dy * Math.sin(vertical));
		// System.out.println(vertical / Math.PI * 180 + " " + antiAngle / Math.PI * 180);
		return k;
	}

	Mat convertContourToMat(MatOfPoint contour) {
		Point[] pts = contour.toArray();
		Mat result = new Mat(pts.length, 2, CvType.CV_64FC1);
		for (int i = 0; i < result.rows(); ++i) {
			result.put(i, 0, pts[i].x);
			result.put(i, 1, pts[i].y);
		}
		return result;
	}

	@Override
	public int compareTo(SuperContour c) {
		int xCompare = Double.compare(center.x, c.center.x);
		return xCompare != 0 ? xCompare : Double.compare(center.y, c.center.y);
	}
}