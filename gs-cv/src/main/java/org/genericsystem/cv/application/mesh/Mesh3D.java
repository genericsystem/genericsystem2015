package org.genericsystem.cv.application.mesh;

import java.util.Arrays;
import java.util.List;
import java.util.stream.DoubleStream;

import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Point3;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

public class Mesh3D extends AbstractMesh<Point3> {

	private final Mesh mesh;
	private List<Point3> points3D;

	public Mesh3D(Mesh mesh) {
		super(mesh.halfWidth, mesh.halfHeight);
		this.mesh = mesh;
		int[][] indexRects = mesh.values().stream().map(indexedPts -> new int[] { indexedPts[0].getIndex(), indexedPts[1].getIndex(), indexedPts[2].getIndex(), indexedPts[3].getIndex() }).toArray(int[][]::new);
		points3D = Svd.solve(mesh.getPointIndex(), indexRects);
		for (int i = -halfHeight; i < halfHeight; i++)
			for (int j = -halfWidth; j < halfWidth; j++) {
				Point3[] para3D = new Point3[4];
				for (int k = 0; k < 4; k++)
					para3D[k] = points3D.get(mesh.get(i, j)[k].getIndex());
				put(i, j, para3D);
			}
	}

	public Mat draw3Dsurface(Scalar colorStart, Scalar colorEnd, Size size) {
		double xMin = points3D.stream().mapToDouble(p -> p.x + p.z).min().getAsDouble();
		double yMin = points3D.stream().mapToDouble(p -> p.y + p.z).min().getAsDouble();
		double zMin = points3D.stream().mapToDouble(p -> p.z).min().getAsDouble();
		double xMax = points3D.stream().mapToDouble(p -> p.x + p.z).max().getAsDouble();
		double yMax = points3D.stream().mapToDouble(p -> p.y + p.z).max().getAsDouble();
		double zMax = points3D.stream().mapToDouble(p -> p.z).max().getAsDouble();

		int newWidth = (int) size.width;
		int newHeight = (int) Math.ceil((yMax - yMin) * size.width / (xMax - xMin));
		Mat result = new Mat(newHeight, newWidth, CvType.CV_8UC3, new Scalar(0, 0, 0));
		for (Point3[] cell3D : values()) {
			Point[] pts = Arrays.stream(cell3D).map(p -> build2DPoint(p, 0, result.width(), 0, result.height(), xMin, xMax, yMin, yMax)).toArray(Point[]::new);
			double lambda = (cell3D[0].z - zMin) / (zMax - zMin);
			Scalar color = combine(colorStart, colorEnd, lambda);
			drawPolygon(result, pts, color, color);
		}
		return result;
	}

	private Point build2DPoint(Point3 p, double xMin, double xMax, double yMin, double yMax, double xMinOrig, double xMaxOrig, double yMinOrig, double yMaxOrig) {
		return new Point(normalize(p.x + p.z, xMin, xMax, xMinOrig, xMaxOrig), normalize(p.y + p.z, yMin, yMax, yMinOrig, yMaxOrig));
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

	public void normalize(double[] sizes, double originalWidth) {
		double width = DoubleStream.of(sizes).sum();
		double coefWidth = originalWidth / width;
		for (int i = 0; i < sizes.length; i++)
			sizes[i] *= coefWidth;
	}

	public double[] getWidths() {
		double[] widths = new double[halfWidth * 2];
		for (int j = 0; j < widths.length; j++) {
			double sum = 0;
			for (int i = 0; i < halfHeight * 2 - 1; i++) {
				Point3[] para = get(i - halfHeight, j - halfWidth);
				assert para != null : "i:" + (i - halfHeight) + " j: " + (j - halfWidth);
				sum += euclideanDistance(para[0], para[1]);
			}
			// Last line, bottom edge.
			Point3[] para = get(halfHeight * 2 - 1 - halfHeight, j - halfWidth);
			sum += euclideanDistance(para[2], para[3]);
			widths[j] = sum / (halfHeight * 2);
		}
		return widths;
	}

	public double[] getHeights() {
		double[] heights = new double[halfHeight * 2];
		for (int i = 0; i < heights.length; i++) {
			double sum = 0;
			for (int j = 0; j < halfWidth * 2 - 1; j++) {
				Point3[] para = get(i - halfHeight, j - halfWidth);
				sum += euclideanDistance(para[0], para[3]);
			}
			// Last column, right edge.
			Point3[] para = get(i - halfHeight, halfWidth * 2 - 1 - halfWidth);
			sum += euclideanDistance(para[1], para[2]);
			heights[i] = sum / (halfWidth * 2);
		}
		return heights;
	}

	public Mat dewarp(Mat src, Size originalSize) {
		double[] heights = getHeights();
		normalize(heights, originalSize.height);
		double[] widths = getWidths();
		normalize(widths, originalSize.width);

		System.out.println(Arrays.toString(heights));
		System.out.println(Arrays.toString(widths));
		System.out.println(DoubleStream.of(heights).sum());
		System.out.println(DoubleStream.of(widths).sum());
		// double[] widths = getWidths(originalSize.width);
		// double[] heights = getHeights(originalSize.height);
		//
		// System.out.println(Arrays.toString(heights));
		// System.out.println(Arrays.toString(widths));
		// System.out.println(DoubleStream.of(heights).sum());
		// System.out.println(DoubleStream.of(widths).sum());

		Mat enlargedImage = new Mat(src.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		double y = enlargedImage.height() / 2;
		for (int i = 0; i < halfHeight; i++) {
			double x = enlargedImage.width() / 2;
			for (int j = 0; j < halfWidth; j++) {
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(src, enlargedImage, mesh.getPoints(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}

			x = enlargedImage.width() / 2;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(src, enlargedImage, mesh.getPoints(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
			y += heights[i + halfHeight];
		}
		y = enlargedImage.height() / 2;
		for (int i = -1; i >= -halfHeight; i--) {
			y -= heights[i + halfHeight];
			double x = enlargedImage.width() / 2;
			for (int j = 0; j < halfWidth; j++) {
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(src, enlargedImage, mesh.getPoints(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}
			x = enlargedImage.width() / 2;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				if (x >= 0 && (x + widths[j + halfWidth] < enlargedImage.width()) && y >= 0 && (y + heights[i + halfHeight] < enlargedImage.height()))
					deWarp(src, enlargedImage, mesh.getPoints(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
		}
		double xBorder = (enlargedImage.width() - originalSize.width) / 2;
		double yBorder = (enlargedImage.height() - originalSize.height) / 2;
		return new Mat(enlargedImage, new Rect(new Point(xBorder, yBorder), new Point(enlargedImage.width() - xBorder, enlargedImage.height() - yBorder)));
	}

	private double euclideanDistance(Point3 p1, Point3 p2) {
		return Math.sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y) + (p2.z - p1.z) * (p2.z - p1.z));
	}

}