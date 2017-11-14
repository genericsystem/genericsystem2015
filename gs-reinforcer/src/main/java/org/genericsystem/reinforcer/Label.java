package org.genericsystem.reinforcer;

import java.text.Collator;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;

public class Label implements Comparable<Label> {
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

	public boolean matchesText(String text) {
		return StringCompare.similar(text, label, SIMILARITY.LEVENSHTEIN);
	}

	// Returns true if <this> is <direction> of <other>.
	public boolean isInDirection(Label other, Direction direction) {
		GSPoint center = getRect().getCenter();
		GSRect otherRect = other.getRect();
		boolean result = true;
		if (direction == Direction.NORTH_WEST || direction == Direction.NORTH || direction == Direction.NORTH_EAST) {
			result &= center.getY() < otherRect.getY();
		}
		if (direction == Direction.NORTH_EAST || direction == Direction.EAST || direction == Direction.SOUTH_EAST) {
			result &= center.getX() > otherRect.getX() + otherRect.getWidth();
		}
		if (direction == Direction.SOUTH_EAST || direction == Direction.SOUTH || direction == Direction.SOUTH_WEST) {
			result &= center.getY() > otherRect.getY() + otherRect.getHeight();
		}
		if (direction == Direction.SOUTH_WEST || direction == Direction.WEST || direction == Direction.NORTH_WEST) {
			result &= center.getX() < otherRect.getX();
		}
		return result;
	}

	public boolean alignedWith(Label other, Alignment alignment) {
		double tolerance = .05;
		double xThis, xOther;
		if (alignment == Alignment.LEFT) {
			xThis = rect.getX();
			xOther = other.rect.getX();
		} else {
			xThis = rect.getX() + rect.getWidth();
			xOther = other.rect.getX() + other.getRect().getWidth();
		}
		return Math.abs(xThis - xOther) < tolerance * Math.max(xThis, xOther);
	}

	@Override
	public int compareTo(Label o) {
		if (this == o)
			return 0;
		int rectDiff = rect.compareTo(o.rect);
		if (rectDiff != 0)
			return rectDiff;
		return Collator.getInstance().compare(label, o.label);
	}
}