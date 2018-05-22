package org.genericsystem.cv.application.mesh;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

import java.util.Collection;
import java.util.HashMap;

public abstract class AbstractMesh<T> {
	protected final int halfWidth;
	protected final int halfHeight;
	private final HashMap<Key, T[]> internal = new HashMap<>();

	public AbstractMesh(int halfWidth, int halfHeight) {
		this.halfHeight = halfHeight;
		this.halfWidth = halfWidth;
	}

	public Collection<T[]> values() {
		return internal.values();
	}

	protected final void deWarp(Mat src, Mat dewarpedImage, Point[] polygon, double x, double y, double rectWidth, double rectHeight) {
		if (inBordedImage(src, polygon)) {
			Rect subImageRect = subImageRect(polygon);
			if (!subImageRect.empty()) {
				Mat homography = dewarpPolygon(polygon, subImageRect, rectHeight, rectWidth);
				Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectWidth, y + rectHeight)));
				Mat subImage = new Mat(src, subImageRect);
				Imgproc.warpPerspective(subImage, subDewarpedImage, homography, subDewarpedImage.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
				subImage.release();
				subDewarpedImage.release();
				homography.release();
			}
		}
	}

	private boolean inBordedImage(Mat img, Point[] pts) {
		for (Point p : pts)
			if (p.x < 0 || p.x >= img.width() || p.y < 0 || p.y >= img.height())
				return false;
		return true;
	}

	private Rect subImageRect(Point[] polygon) {
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

	private Point changeOrigin(Rect subImageRect, Point point) {
		return new Point(point.x - subImageRect.x, point.y - subImageRect.y);
	}

	// this.mesh = new Point[2 * halfHeight][2 * halfWidth][4];
	public final void put(int i, int j, T... points) {
		internal.put(new Key(i, j), points);
	}

	public final T[] get(int i, int j) {
		return internal.get(new Key(i, j));
		// return internal.get(new Key(i + halfHeight, j + halfWidth));
	}

}