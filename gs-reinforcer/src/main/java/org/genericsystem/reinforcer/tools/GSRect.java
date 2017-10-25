package org.genericsystem.reinforcer.tools;

public class GSRect {
	private double x, y, width, height;

	public GSRect(double x, double y, double width, double height) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}

	public GSRect() {
		this(0, 0, 0, 0);
	}

	public GSRect(GSPoint p1, GSPoint p2) {
		x = p1.getX() < p2.getX() ? p1.getX() : p2.getX();
		y = p1.getY() < p2.getY() ? p1.getY() : p2.getY();
		width = (p1.getX() > p2.getX() ? p1.getX() : p2.getX()) - x;
		height = (p1.getY() > p2.getY() ? p1.getY() : p2.getY()) - y;
	}

	public GSRect(GSPoint p, GSSize s) {
		this(p.getX(), p.getY(), s.getWidth(), s.getHeight());
	}

	public GSRect(double[] vals) {
		set(vals);
	}

	private void set(double[] vals) {
		if (vals != null) {
			x = vals.length > 0 ? vals[0] : 0;
			y = vals.length > 1 ? vals[1] : 0;
			width = vals.length > 2 ? vals[2] : 0;
			height = vals.length > 3 ? vals[3] : 0;
		} else {
			x = 0;
			y = 0;
			width = 0;
			height = 0;
		}
	}

	@Override
	public GSRect clone() {
		return new GSRect(x, y, width, height);
	}

	public GSPoint tl() {
		return new GSPoint(x, y);
	}

	public GSPoint br() {
		return new GSPoint(x + width, y + height);
	}

	public GSSize size() {
		return new GSSize(width, height);
	}

	public double area() {
		return width * height;
	}

	public boolean empty() {
		return width <= 0 || height <= 0;
	}

	public boolean contains(GSPoint p) {
		return x <= p.getX() && p.getX() < x + width && y <= p.getY() && p.getY() < y + height;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(height);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(width);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(x);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(y);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof GSRect))
			return false;
		GSRect it = (GSRect) obj;
		return x == it.x && y == it.y && width == it.width && height == it.height;
	}

	@Override
	public String toString() {
		return "{" + x + ", " + y + ", " + width + "x" + height + "}";
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

	public double getWidth() {
		return width;
	}

	public double getHeight() {
		return height;
	}
}
