package org.genericsystem.reinforcer;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.GSSize;

public class NormalizedRect extends GSRect {

	public NormalizedRect(double x, double y, double width, double height) {
		super(x, y, width, height);
		checkCoordinates(x, y, width, height);
	}

	public NormalizedRect() {
		this(0, 0, 0, 0);
	}

	public NormalizedRect(GSPoint p1, GSPoint p2) {
		super(p1, p2);
		double x = p1.getX() < p2.getX() ? p1.getX() : p2.getX();
		double y = p1.getY() < p2.getY() ? p1.getY() : p2.getY();
		double width = (p1.getX() > p2.getX() ? p1.getX() : p2.getX()) - x;
		double height = (p1.getY() > p2.getY() ? p1.getY() : p2.getY()) - y;
		checkCoordinates(x, y, width, height);
	}

	public NormalizedRect(GSPoint p, GSSize s) {
		this(p.getX(), p.getY(), s.getWidth(), s.getHeight());
	}

	private void checkCoordinates(double x, double y, double width, double height) {
		if (x < 0 || x > 1 || y < 0 || y > 1 || x + width > 1 || y + height > 1)
			throw new IllegalStateException("The coordinates of all vertices of a normalized rectangle must be between 0 and 1."
					+ " Min x: " + x + ", max x: " + x + width + ", min y: " + y + ", max y: " + y + height);
	}
}
