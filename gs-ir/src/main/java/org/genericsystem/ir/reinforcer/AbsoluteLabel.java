package org.genericsystem.ir.reinforcer;

public class AbsoluteLabel {
	private final double tlx, tly, brx, bry;
	private final String label;

	public AbsoluteLabel(double tlx, double tly, double brx, double bry, String label) {
		assert tlx < brx && tly < bry;
		this.tlx = tlx;
		this.tly = tly;
		this.brx = brx;
		this.bry = bry;
		this.label = label;
	}

	@Override
	public String toString() {
		return "(" + tlx + "," + tly + ")" + "(" + brx + "," + bry + ") " + label;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AbsoluteLabel))
			return false;
		AbsoluteLabel al = (AbsoluteLabel) obj;
		return al.tlx == tlx && al.tly == tly && al.brx == brx && al.bry == bry && label.equals(al.label);
	}

	@Override
	public int hashCode() {
		return label.hashCode();
	}

	public boolean intersectWith(AbsoluteLabel candidate) {
		double maxtlx = Math.max(tlx, candidate.tlx);
		double maxtly = Math.max(tly, candidate.tly);
		double minbrx = Math.min(brx, candidate.brx);
		double minbry = Math.min(bry, candidate.bry);
		return maxtlx <= minbrx || maxtly <= minbry;
		// (maxtlx,maxtly) - (minbrx,minbry)

		// return !(tlx < candidate.brx && brx > candidate.tlx && tly > candidate.bry && bry < candidate.tly);
	}
}