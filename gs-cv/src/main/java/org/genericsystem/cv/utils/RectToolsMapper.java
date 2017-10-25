package org.genericsystem.cv.utils;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.genericsystem.reinforcer.tools.RectangleTools.MERGE_METHOD;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;

/**
 * This is an adaptater class for gs-reinforcer {@link RectangleTools}, that accept of OpenCV's {@link Rect}, {@link Point} and {@link Size}.
 * 
 * @author Pierrik Lassalas
 */
public final class RectToolsMapper {

	private static final Function<? super Rect, ? extends GSRect> rectToGSRect = rect -> convert(rect);
	private static final Function<? super GSRect, ? extends Rect> gsRectToRect = gsRect -> convert(gsRect);
	private static final Function<? super Point, ? extends GSPoint> pointToGSPoint = point -> convert(point);
	private static final Function<? super GSPoint, ? extends Point> gsPointToPoint = gsPoint -> convert(gsPoint);

	private static GSRect convert(Rect rect) {
		return new GSRect(rect.x, rect.y, rect.width, rect.height);
	}

	private static Rect convert(GSRect gsRect) {
		return new Rect((int) gsRect.getX(), (int) gsRect.getY(), (int) gsRect.getWidth(), (int) gsRect.getHeight());
	}

	private static Point convert(GSPoint gsPoint) {
		return new Point(gsPoint.getX(), gsPoint.getY());
	}

	private static GSPoint convert(Point point) {
		return new GSPoint(point.x, point.y);
	}

	public static List<Rect> groupRectangles(List<Rect> input, MERGE_METHOD method) {
		List<GSRect> rects = input.stream().map(rectToGSRect).collect(Collectors.toList());
		List<GSRect> result = RectangleTools.groupRectangles(rects, method);
		return result.stream().map(gsRectToRect).collect(Collectors.toList());
	}

	public static List<Rect> groupRectangles(List<Rect> input, double eps, double groupThreshold, MERGE_METHOD method) {
		List<GSRect> rects = input.stream().map(rectToGSRect).collect(Collectors.toList());
		List<GSRect> result = RectangleTools.groupRectangles(rects, eps, groupThreshold, method);
		return result.stream().map(gsRectToRect).collect(Collectors.toList());
	}

	public static List<List<Rect>> cluster(List<Rect> input, double eps) {
		List<GSRect> rects = input.stream().map(rectToGSRect).collect(Collectors.toList());
		List<List<GSRect>> result = RectangleTools.cluster(rects, eps);
		return result.stream().map(list -> (List<Rect>) list.stream().map(gsRectToRect).collect(Collectors.toList())).collect(Collectors.toList());
	}

	public static boolean isInCluster(Rect rect1, Rect rect2, double eps) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		return RectangleTools.isInCluster(r1, r2, eps);
	}

	public static double[] commonArea(Rect rect1, Rect rect2) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		return RectangleTools.commonArea(r1, r2);
	}

	public static double inclusiveArea(Rect rect1, Rect rect2) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		return RectangleTools.inclusiveArea(r1, r2);
	}

	public static Optional<Rect> getIntersection(Rect rect1, Rect rect2) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		Optional<GSRect> optional = RectangleTools.getIntersection(r1, r2);
		return optional.map(gsRectToRect);
	}

	public static Rect getUnion(Rect rect1, Rect rect2) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		GSRect result = RectangleTools.getUnion(r1, r2);
		return convert(result);
	}

	public static Rect getMean(List<Rect> rects) {
		List<GSRect> rectangles = rects.stream().map(rectToGSRect).collect(Collectors.toList());
		GSRect result = RectangleTools.getMean(rectangles);
		return convert(result);
	}

	public static boolean isOverlapping(Rect rect1, Rect rect2) throws IllegalArgumentException {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		return RectangleTools.isOverlapping(r1, r2);
	}

	public static Optional<Rect> getInsider(Rect rect1, Rect rect2) {
		GSRect r1 = convert(rect1);
		GSRect r2 = convert(rect2);
		Optional<GSRect> optional = RectangleTools.getInsider(r1, r2);
		return optional.map(gsRectToRect);
	}

	public static boolean contains(Rect rect, Point point) {
		GSRect r = convert(rect);
		GSPoint p = convert(point);
		return RectangleTools.contains(r, p);
	}

	public static Point[] decomposeClockwise(Rect rect) {
		GSRect r = convert(rect);
		List<GSPoint> points = Arrays.asList(RectangleTools.decomposeClockwise(r));
		return points.stream().map(gsPointToPoint).toArray(Point[]::new);
	}

	public static List<Rect> nonMaximumSuppression(List<Rect> boxes, double overlapThreshold) {
		List<GSRect> rectangles = boxes.stream().map(rectToGSRect).collect(Collectors.toList());
		List<GSRect> results = RectangleTools.nonMaximumSuppression(rectangles, overlapThreshold);
		return results.stream().map(gsRectToRect).collect(Collectors.toList());
	}

}
