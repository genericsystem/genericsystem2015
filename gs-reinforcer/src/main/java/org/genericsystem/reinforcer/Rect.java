package org.genericsystem.reinforcer;

public class Rect {
	private int x, y, width, height;

	public Rect(int x, int y, int width, int height) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}

	public Rect() {
		this(0, 0, 0, 0);
	}

	public Rect(Point p1, Point p2) {
		x = (int) (p1.getX() < p2.getX() ? p1.getX() : p2.getX());
		y = (int) (p1.getY() < p2.getY() ? p1.getY() : p2.getY());
		width = (int) (p1.getX() > p2.getX() ? p1.getX() : p2.getX()) - x;
		height = (int) (p1.getY() > p2.getY() ? p1.getY() : p2.getY()) - y;
	}

	public Rect(Point p, Size s) {
		this((int) p.getX(), (int) p.getY(), (int) s.getWidth(), (int) s.getHeight());
	}

	public Rect(double[] vals) {
		set(vals);
	}

	private void set(double[] vals) {
		if (vals != null) {
			x = vals.length > 0 ? (int) vals[0] : 0;
			y = vals.length > 1 ? (int) vals[1] : 0;
			width = vals.length > 2 ? (int) vals[2] : 0;
			height = vals.length > 3 ? (int) vals[3] : 0;
		} else {
			x = 0;
			y = 0;
			width = 0;
			height = 0;
		}
	}

	@Override
	public Rect clone() {
		return new Rect(x, y, width, height);
	}

	public Point tl() {
		return new Point(x, y);
	}

	public Point br() {
		return new Point(x + width, y + height);
	}

	public Size size() {
		return new Size(width, height);
	}

	public double area() {
		return width * height;
	}

	public boolean empty() {
		return width <= 0 || height <= 0;
	}

	public boolean contains(Point p) {
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
		if (!(obj instanceof Rect))
			return false;
		Rect it = (Rect) obj;
		return x == it.x && y == it.y && width == it.width && height == it.height;
	}

	@Override
	public String toString() {
		return "{" + x + ", " + y + ", " + width + "x" + height + "}";
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}
}
