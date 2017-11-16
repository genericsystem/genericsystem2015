package org.genericsystem.cv.utils;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;

/**
 * This is an adapter class for gs-reinforcer {@link RectangleTools}, that provides translation between OpenCV's {@link Rect}, {@link Point} and {@link Size} and their GS counterparts.
 */
public final class RectToolsMapper {

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

	public static List<Rect> gsRectToRect(List<GSRect> gsRects) {
		return gsRectToRectStream(gsRects).collect(Collectors.toList());
	}

	public static Stream<Rect> gsRectToRectStream(List<GSRect> gsRects) {
		return gsRects.stream().map(RectToolsMapper::convert);
	}

	public static List<GSRect> rectToGSRect(List<Rect> rects) {
		return rectToGSRectStream(rects).collect(Collectors.toList());
	}

	public static Stream<GSRect> rectToGSRectStream(List<Rect> rects) {
		return rects.stream().map(RectToolsMapper::convert);
	}

	public static List<Point> gsPointToPoint(List<GSPoint> gsPoints) {
		return gsPointToPointStream(gsPoints).collect(Collectors.toList());
	}

	public static Stream<Point> gsPointToPointStream(List<GSPoint> gsPoints) {
		return gsPoints.stream().map(RectToolsMapper::convert);
	}

	public static List<GSPoint> pointToGSPoint(List<Point> points) {
		return pointToGSPointStream(points).collect(Collectors.toList());
	}

	public static Stream<GSPoint> pointToGSPointStream(List<Point> points) {
		return points.stream().map(RectToolsMapper::convert);
	}

}
