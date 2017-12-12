package org.genericsystem.cv.retriever;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class FieldDrawer {

	public void drawFieldsOnStabilized(Img stabilizedDisplay, Fields fields) {
		fields.stream().forEach(field -> draw(stabilizedDisplay, field, 1));
	}

	public void drawOcrPerspectiveInverse(Img display, Fields fields, Mat homography, int thickness) {
		fields.stream().forEach(field -> drawWithHomography(display, field, homography, thickness));
	}

	public void drawWithHomography(Img display, Field field, Mat homography, int thickness) {
		if (needRect(display, field)) {
			drawRect(display, getRectPointsWithHomography(homography, field), drawAsLocked(field) ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
			drawText(display, field, getRectPointsWithHomography(homography, field), new Scalar(0, 255, 0), thickness);
		}
	}

	private Point[] getRectPointsWithHomography(Mat homography, Field field) {
		List<Point> points = RectToolsMapper.gsPointToPoint(Arrays.asList(field.getRect().decomposeClockwise()));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
		return results.toArray();
	}

	private boolean drawAsLocked(Field field) {
		return field.isOrphan() || (!field.isOrphan() && field.getParent().getDeadCounter() != 0) ? field.isLocked() : false;
	}

	public void draw(Img display, Field field, int thickness) {
		if (needText(display, field))
			drawDebugText(display, field, new Scalar(0, 64, 255), thickness);
		drawRect(display, field, field.getDeadCounter() == 0 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
	}

	private boolean needRect(Img display, Field field) {
		GSRect imgRect = new GSRect(0, 0, display.width(), display.height());
		return !field.isOrphan() && field.getParent().getRect().isInside(imgRect) ? false : field.getRect().isInside(imgRect);
	}

	private boolean needText(Img display, Field field) {
		return field.getDeadCounter() == 0 && needRect(display, field);
	}

	public void drawDebugText(Img display, Point[] targets, Field field, Scalar color, int thickness) {
		String conf = String.format("%.3f", field.getLockLevel());
		Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
		double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
		Imgproc.putText(display.getSrc(), conf, new Point(topCenter.x - l, topCenter.y - 12), Core.FONT_HERSHEY_TRIPLEX, 0.35, color);
	}

	public void drawDebugText(Img display, Field field, Scalar color, int thickness) {
		Point[] points = RectToolsMapper.gsPointToPoint(Arrays.asList(field.getRect().decomposeClockwise())).toArray(new Point[0]);
		drawDebugText(display, points, field, color, thickness);
	}

	public void drawRect(Img stabilizedDisplay, Field field, Scalar color, int thickness) {
		Point[] points = RectToolsMapper.gsPointToPoint(Arrays.asList(field.getRect().decomposeClockwise())).toArray(new Point[0]);
		drawRect(stabilizedDisplay, points, color, thickness);
	}

	public void drawRect(Img display, Point[] targets, Scalar color, int thickness) {
		for (int i = 0; i < targets.length; ++i)
			Imgproc.line(display.getSrc(), targets[i], targets[(i + 1) % targets.length], color, thickness);
	}

	public void drawText(Img display, Field field, Point[] targets, Scalar color, int thickness) {		
		if (field.getConsolidated() != null) {			
			String text = Normalizer.normalize(field.getConsolidated(), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
			// --- //
			Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
			double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
			Imgproc.line(display.getSrc(), new Point(topCenter.x, topCenter.y - 2), new Point(topCenter.x, topCenter.y - 12), color, 1);
			Imgproc.putText(display.getSrc(), text, new Point(topCenter.x - l, topCenter.y - 14), Core.FONT_HERSHEY_TRIPLEX, 0.45, color, 1);
		}
	}

	public void drawFieldsOnStabilizedDebug(Img stabilized, Fields fields) {
		for (GSRect rect : fields.mergeRectsList(stabilized)) {
			Point[] targets = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
			for (int i = 0; i < targets.length; ++i)
				Imgproc.line(stabilized.getSrc(), targets[i], targets[(i + 1) % targets.length], new Scalar(255, 0, 0), 1);
		}
	}

}
