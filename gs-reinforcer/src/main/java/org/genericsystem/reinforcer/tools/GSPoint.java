package org.genericsystem.reinforcer.tools;

public class GSPoint {
	private double x, y;

	public GSPoint(double x, double y) {
		this.x = x;
		this.y = y;
	}

	public GSPoint() {
		this(0, 0);
	}

	public GSPoint(double[] vals) {
		this();
		set(vals);
	}

	private void set(double[] vals) {
		if (vals != null) {
			x = vals.length > 0 ? vals[0] : 0;
			y = vals.length > 1 ? vals[1] : 0;
		} else {
			x = 0;
			y = 0;
		}
	}

	@Override
	public GSPoint clone() {
		return new GSPoint(x, y);
	}

	public double dot(GSPoint p) {
		return x * p.x + y * p.y;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
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
		if (!(obj instanceof GSPoint))
			return false;
		GSPoint it = (GSPoint) obj;
		return x == it.x && y == it.y;
	}

	public boolean inside(GSRect r) {
		return r.contains(this);
	}

	@Override
	public String toString() {
		return "{" + x + ", " + y + "}";
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}
}
