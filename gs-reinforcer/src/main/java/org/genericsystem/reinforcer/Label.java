package org.genericsystem.reinforcer;

import org.genericsystem.reinforcer.tools.GSRect;

public class Label {
	private final GSRect rect;
	private final String label;

	public Label(double tlx, double tly, double brx, double bry, String label) {
		assert tlx < brx && tly < bry : "tlx: " + tlx + ", brx: " + brx + ", tly: " + tly + ", bry: " + bry;
		rect = new GSRect(tlx, tly, brx - tlx, bry - tly);
		this.label = label;
	}

	public Label(GSRect rect, String label) {
		this.rect = rect;
		this.label = label;
	}

	@Override
	public String toString() {
		return "{Label " + rect.toString() + ", content: " + label + "}";
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Label))
			return false;
		Label al = (Label) obj;
		return al.rect.equals(rect) && label.equals(al.label);
	}

	@Override
	public int hashCode() {
		return label.hashCode();
	}

	public GSRect getRect() {
		return rect;
	}

	public String getText() {
		return label;
	}

	public Label normalize(double mintlx, double mintly, double width, double height) {
		return new Label(rect.normalize(mintlx, mintly, width, height), label);
	}
}