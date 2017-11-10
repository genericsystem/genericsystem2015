package org.genericsystem.cv.utils;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;

/**
 * This is an adapter class for gs-reinforcer {@link RectangleTools}, that provides translation between OpenCV's {@link Rect}, {@link Point} and {@link Size} and their GS counterparts.
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

	public static List<Rect> gsRectToRect(List<GSRect> gsRects) {
		return gsRects.stream().map(gsRectToRect).collect(Collectors.toList());
	}

	public static List<GSRect> rectToGSRect(List<Rect> rects) {
		return rects.stream().map(rectToGSRect).collect(Collectors.toList());
	}

	public static List<Point> gsPointToPoint(List<GSPoint> gsPoints) {
		return gsPoints.stream().map(gsPointToPoint).collect(Collectors.toList());
	}

	public static List<GSPoint> pointToGSPoint(List<Point> points) {
		return points.stream().map(pointToGSPoint).collect(Collectors.toList());
	}

}
