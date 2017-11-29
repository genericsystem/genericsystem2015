package org.genericsystem.reinforcer;

import java.text.Collator;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
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

	public Label affineTransform(double xScale, double xShift, double yScale, double yShift) {
		double newX = xScale * rect.getX() + xShift;
		double newBrX = xScale * rect.br().getX() + xShift;
		double newY = yScale * rect.getY() + yShift;
		double newBrY = yScale * rect.br().getY() + yShift;
		return new Label(newX, newY, newBrX, newBrY, label);
	}

	public boolean matchesText(String text) {
		return StringCompare.similar(text, label, SIMILARITY.LEVENSHTEIN);
	}

	// Returns true if <this> is <direction> of <other>.
	public boolean isInDirection(Label other, Direction direction) {
		GSPoint center = getRect().getCenter();
		GSRect otherRect = other.getRect();
		boolean north = center.getY() < otherRect.getY(); 
		boolean east = center.getX() > otherRect.getX() + otherRect.getWidth();
		boolean south = center.getY() > otherRect.getY() + otherRect.getHeight();
		boolean west = center.getX() < otherRect.getX();
		switch (direction) {
			case NORTH_EAST:
				return east && north;
			case NORTH:
				return north && !east && !west;
			case NORTH_WEST:
				return north && west;
			case WEST:
				return west && !south && !north;
			case SOUTH_WEST:
				return south && west;
			case SOUTH:
				return south && !west && !east;
			case SOUTH_EAST:
				return south && east;
			case EAST:
			default:
				return east && !south && !north;
		}
	}

	public double alignmentCost(Label other) {
		double result;
		if (RectangleTools.isInCluster(rect, other.rect, RectangleTools.DEFAULT_EPSILON))
			result = 0;
		else if (alignedWith(other, Alignment.LEFT) || alignedWith(other, Alignment.RIGHT))
			result = .4;
		else
			result = .8;
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