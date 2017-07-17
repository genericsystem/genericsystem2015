package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;

public class Shard {

	private double x1;
	private double x2;
	private double y1;
	private double y2;

	private String label;
	private List<Shard> children = new ArrayList<Shard>();
	private Shard parent = null;

	public Shard(double x1, double x2, double y1, double y2) {
		this.x1 = x1;
		this.x2 = x2;
		this.y1 = y1;
		this.y2 = y2;
	}

	public Shard() {

	}

	public void addChild(Shard child) {
		if (!children.contains(child))
			children.add(child);
	}

	public void removeChild(Shard child) {
		if (children.contains(child))
			children.remove(child);
	}

	public boolean equiv(Shard s, double xTolerance, double yTolerance) {

		if (Math.abs(s.x1 - x1) < xTolerance && Math.abs(s.x2 - x2) < xTolerance && Math.abs(s.y1 - y1) < yTolerance && Math.abs(s.y2 - y2) < yTolerance)
			return true;

		return false;
	}

	public List<Shard> getChildren() {
		return children;
	}

	public double getX1() {
		return x1;
	}

	public void setX1(double x1) {
		this.x1 = x1;
	}

	public double getX2() {
		return x2;
	}

	public void setX2(double x2) {
		this.x2 = x2;
	}

	public double getY1() {
		return y1;
	}

	public void setY1(double y1) {
		this.y1 = y1;
	}

	public double getY2() {
		return y2;
	}

	public void setY2(double y2) {
		this.y2 = y2;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public Shard getParent() {
		return parent;
	}

	public void setParent(Shard parent) {
		this.parent = parent;
	}

	public boolean hasChildren() {

		if (children != null && children.size() >= 1)
			return true;

		return false;
	}

	@Override
	public String toString() {

		StringBuilder sb = new StringBuilder();
		int depth = 0;
		toString(this, sb, depth);
		return sb.toString();
	}

	private void toString(Shard shard, StringBuilder sb, int depth) {
		sb.append("depth : " + depth + " : ");
		sb.append("((" + shard.x1 + "-" + shard.x2 + "),(" + shard.y1 + "-" + shard.y2 + "))".toString());

		if (shard.hasChildren()) {
			depth++;
			for (Shard s : shard.getChildren()) {
				sb.append("\n");
				for (int i = 0; i < depth; i++)
					sb.append("   ");
				toString(s, sb, depth);
			}
		}
	}

}
