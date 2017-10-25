package org.genericsystem.ir.reinforcer;

public class Label {
	private final double tlx, tly, brx, bry;
	private final String label;

	public Label(double tlx, double tly, double brx, double bry, String label) {
		assert tlx < brx && tly < bry;
		this.tlx = tlx;
		this.tly = tly;
		this.brx = brx;
		this.bry = bry;
		this.label = label;
	}

	@Override
	public String toString() {
		return String.format("tl(%.1f, %.1f), br(%.1f, %.1f), %s", tlx, tly, brx, bry, label);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Label))
			return false;
		Label al = (Label) obj;
		return al.tlx == tlx && al.tly == tly && al.brx == brx && al.bry == bry && label.equals(al.label);
	}

	@Override
	public int hashCode() {
		return label.hashCode();
	}

	public double getTlx() {
		return tlx;
	}

	public double getTly() {
		return tly;
	}

	public double getBrx() {
		return brx;
	}

	public double getBry() {
		return bry;
	}

	public boolean intersectWith(Label candidate) {
		return tlx < candidate.brx && candidate.tlx < brx && tly < candidate.bry && candidate.bry < bry;
	}

		// return !(tlx < candidate.brx && brx > candidate.tlx && tly > candidate.bry && bry < candidate.tly);
	}

	public Label normalize(double mintlx, double mintly, double width, double height) {
		System.out.println(mintlx + " " + mintly + " " + width + " " + height);
		return new Label((tlx - mintlx) / width, (tly - mintly) / height, (brx - mintlx) / width, (bry - mintly) / height, label);
	}
}