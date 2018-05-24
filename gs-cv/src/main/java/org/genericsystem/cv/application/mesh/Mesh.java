package org.genericsystem.cv.application.mesh;

import java.util.List;

import org.genericsystem.cv.application.mesh.Points.IndexedPoint;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

class Mesh extends AbstractMesh<IndexedPoint> {

	private final Points points;

	public Mesh(Points points, int halfWidth, int halfHeight) {
		super(halfWidth, halfHeight);
		this.points = points;
		for (int i = -halfHeight + 1; i <= halfHeight; i++)
			for (int j = -halfWidth + 1; j <= halfWidth; j++) {
				IndexedPoint leftTop = points.get(i - 1, j - 1);
				IndexedPoint rightTop = points.get(i - 1, j);
				IndexedPoint rightBottom = points.get(i, j);
				IndexedPoint leftBottom = points.get(i, j - 1);
				assert leftTop != null : i + " " + j;
				assert rightTop != null : i + " " + j;
				assert rightBottom != null : i + " " + j;
				assert leftBottom != null : i + " " + j;
				put(i - 1, j - 1, leftTop, rightTop, rightBottom, leftBottom);
			}
	}

	public List<Point> getPointIndex() {
		return points.getPointIndex();
	}

	public void draw(Mat img, Scalar meshColor, Scalar ptsColor) {
		for (int i = -halfHeight; i < halfHeight; i++)
			for (int j = -halfWidth; j < halfWidth; j++)
				drawPolygon(img, getPoints(i, j), meshColor, ptsColor);
	}

	Point[] getPoints(int i, int j) {
		IndexedPoint[] indexedPts = get(i, j);
		return new Point[] { indexedPts[0].getPoint(), indexedPts[1].getPoint(), indexedPts[2].getPoint(), indexedPts[3].getPoint() };
	}

	public Mat dewarp(Mat img, Size originalSize) {
		double rectHeight = originalSize.height / (2 * halfHeight);
		double rectWidth = originalSize.width / (2 * halfWidth);
		Mat dewarpedImage = new Mat(originalSize, CvType.CV_8UC3, new Scalar(255, 255, 255));
		for (int i = -halfHeight; i < halfHeight; i++)
			for (int j = -halfWidth; j < halfWidth; j++)
				deWarp(img, dewarpedImage, getPoints(i, j), (j + halfWidth) * rectWidth, (i + halfHeight) * rectHeight, rectWidth, rectHeight);
		return dewarpedImage;
	}

}