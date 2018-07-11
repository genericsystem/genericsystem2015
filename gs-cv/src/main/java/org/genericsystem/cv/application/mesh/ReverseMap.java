package org.genericsystem.cv.application.mesh;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class ReverseMap {
	private final Map<Range[], Mat> reverseMap = new HashMap<>();

	public ReverseMap(Mesh mesh, int halfWidth, int halfHeight, Point imageCenter, double[] widths, double[] heights) {

		double y = imageCenter.y;
		for (int i = 0; i < halfHeight; i++) {
			double x = imageCenter.x;
			for (int j = 0; j < halfWidth; j++) {
				putReverse(mesh.getCell(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}

			x = imageCenter.x;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				putReverse(mesh.getCell(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
			y += heights[i + halfHeight];
		}
		y = imageCenter.y;

		for (int i = -1; i >= -halfHeight; i--) {
			y -= heights[i + halfHeight];
			double x = imageCenter.x;
			for (int j = 0; j < halfWidth; j++) {
				putReverse(mesh.getCell(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
				x += widths[j + halfWidth];
			}
			x = imageCenter.x;
			for (int j = -1; j >= -halfWidth; j--) {
				x -= widths[j + halfWidth];
				putReverse(mesh.getCell(i, j), x, y, widths[j + halfWidth], heights[i + halfHeight]);
			}
		}

	}

	public Point reverse(Point point) {
		// System.out.println("Reverse : " + point.x + " " + point.y);
		Mat homography = reverseMap.entrySet().stream().filter(entry -> entry.getKey()[0].start <= point.x && entry.getKey()[0].end >= point.x && entry.getKey()[1].start <= point.y && entry.getKey()[1].end >= point.y).map(entry -> entry.getValue())
				.findFirst().get();
		return transform(Arrays.asList(point), homography).get(0);
	}

	private List<Point> transform(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2d_to_Mat(originals);
		Mat results = new Mat();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = new ArrayList<>();
		Converters.Mat_to_vector_Point2d(results, res);
		return res;
	}

	public void putReverse(Point[] polygon, double x, double y, double rectWidth, double rectHeight) {
		Point dewarpedTopLeft = new Point(x, y);
		Point dewarpedTopRight = new Point(x + rectWidth, y);
		Point dewarpedBottomRight = new Point(x + rectWidth, y + rectHeight);
		Point dewarpedBottomLeft = new Point(x, y + rectHeight);
		Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(polygon), new MatOfPoint2f(dewarpedTopLeft, dewarpedTopRight, dewarpedBottomRight, dewarpedBottomLeft));
		reverseMap.put(new Range[] { new Range((int) Math.round(x), (int) Math.round(x + rectWidth)), new Range((int) Math.round(y), (int) Math.round(y + rectHeight)) }, homography.inv());
		// System.out.println(x + " " + y);
	}

}