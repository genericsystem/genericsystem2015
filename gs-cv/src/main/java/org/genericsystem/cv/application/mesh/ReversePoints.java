package org.genericsystem.cv.application.mesh;

import org.opencv.core.Point;
import org.opencv.core.Size;

public class ReversePoints extends Points {

	public ReversePoints(ReverseMap reverseMap, int xBorder, int yBorder, Point imgCenter, int halfWidth, int halfHeight, Size size) {
		super(xBorder, yBorder);
		for (int i = -halfHeight; i <= halfHeight; i++)
			for (int j = -halfWidth; j <= halfWidth; j++)
				put(i, j, createIndexedPoint(reverseMap.reverse(new Point(imgCenter.x + j * (size.width / (2 * halfWidth)), imgCenter.y + i * (size.height / (2 * halfHeight))))));
	}

}
