package org.genericsystem.reinforcer;

public class GSSize {
	private double width, height;

	public GSSize(double width, double height) {
		this.width = width;
		this.height = height;
	}

	public GSSize() {
		this(0, 0);
	}

	public GSSize(GSPoint p) {
		width = p.getX();
		height = p.getY();
	}

	public GSSize(double[] vals) {
		set(vals);
	}

	private void set(double[] vals) {
		if (vals != null) {
			width = vals.length > 0 ? vals[0] : 0;
			height = vals.length > 1 ? vals[1] : 0;
		} else {
			width = 0;
			height = 0;
		}
	}

	public double area() {
		return width * height;
	}

	public boolean empty() {
		return width <= 0 || height <= 0;
	}

	@Override
	public GSSize clone() {
		return new GSSize(width, height);
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
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof GSSize))
			return false;
		GSSize it = (GSSize) obj;
		return width == it.width && height == it.height;
	}

	@Override
	public String toString() {
		return (int) width + "x" + (int) height;
	}

	public double getWidth() {
		return width;
	}

	public double getHeight() {
		return height;
	}
}
