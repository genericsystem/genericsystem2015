package org.genericsystem.cv.application.mesh;

import java.util.Collection;
import java.util.HashMap;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

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
			Point dewarpedTopLeft = new Point(0, 0);
			Point dewarpedTopRight = new Point(rectWidth, 0);
			Point dewarpedBottomRight = new Point(rectWidth, rectHeight);
			Point dewarpedBottomLeft = new Point(0, rectHeight);
			Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(polygon[0], polygon[1], polygon[2], polygon[3]), new MatOfPoint2f(dewarpedTopLeft, dewarpedTopRight, dewarpedBottomRight, dewarpedBottomLeft));
			Mat subDewarpedImage = new Mat(dewarpedImage, new Rect(new Point(x, y), new Point(x + rectWidth, y + rectHeight)));
			Imgproc.warpPerspective(src, subDewarpedImage, homography, subDewarpedImage.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
			subDewarpedImage.release();
			homography.release();
		}
	}

	private boolean inBordedImage(Mat img, Point[] pts) {
		for (Point p : pts)
			if (p.x < 0 || p.x >= img.width() || p.y < 0 || p.y >= img.height())
				return false;
		return true;
	}

	protected void drawPolygon(Mat image, Point[] polygon, Scalar meshColor, Scalar ptsColor) {
		Point topLeft = polygon[0];
		Point topRight = polygon[1];
		Point bottomRight = polygon[2];
		Point bottomLeft = polygon[3];
		Imgproc.circle(image, topLeft, 3, ptsColor, -1);
		Imgproc.circle(image, topRight, 3, ptsColor, -1);
		Imgproc.circle(image, bottomRight, 3, ptsColor, -1);
		Imgproc.circle(image, bottomLeft, 3, ptsColor, -1);
		Imgproc.line(image, topLeft, topRight, meshColor);
		Imgproc.line(image, topRight, bottomRight, meshColor);
		Imgproc.line(image, bottomRight, bottomLeft, meshColor);
		Imgproc.line(image, bottomLeft, topLeft, meshColor);
	}

	public final void put(int i, int j, T... points) {
		internal.put(new Key(i, j), points);
	}

	public final T[] get(int i, int j) {
		return internal.get(new Key(i, j));
		// return internal.get(new Key(i + halfHeight, j + halfWidth));
	}

}