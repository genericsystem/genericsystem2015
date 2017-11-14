package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.reinforcer.Constraint.AlignmentConstraint;
import org.genericsystem.reinforcer.Constraint.PositionConstraint;
import org.genericsystem.reinforcer.Constraint.RelationConstraint;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;

public class Labels implements Iterable<Label> {

	private final Set<Label> labels = new HashSet<>();

	public boolean addLabel(double tlx, double tly, double brx, double bry, String candidateLabel) {
		Label candidate = new Label(tlx, tly, brx, bry, candidateLabel);
		return addLabel(candidate);
	}

	public boolean addLabel(Label candidate) {
		for (Label label : labels)
			if (label.getRect().isOverlappingStrict(candidate.getRect()))
				throw new IllegalStateException(label + " intersect with : " + candidate);
		return labels.add(candidate);
	}

	@Override
	public String toString() {
		return labels.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Labels))
			return false;
		return labels.equals(((Labels) obj).labels);
	}

	@Override
	public int hashCode() {
		return labels.hashCode();
	}

	public Labels normalizeLabels() {
		double mintlx = Double.MAX_VALUE, mintly = Double.MAX_VALUE, maxbrx = 0, maxbry = 0;
		for (GSRect rect : labels.stream().map(l -> l.getRect()).collect(Collectors.toList())) {
			if (rect.getX() < mintlx)
				mintlx = rect.getX();
			if (rect.getY() < mintly)
				mintly = rect.getY();
			if (rect.br().getX() > maxbrx)
				maxbrx = rect.br().getX();
			if (rect.br().getY() > maxbry)
				maxbry = rect.br().getY();
		}
		double width = maxbrx - mintlx;
		double height = maxbry - mintly;
		Labels normalized = new Labels();
		for (Label label : labels)
			normalized.addLabel(label.normalize(mintlx, mintly, width, height));
		return normalized;
	}

	@Override
	public Iterator<Label> iterator() {
		return labels.iterator();
	}

	public List<Label> toList() {
		return stream().collect(Collectors.toList());
	}

	public Stream<Label> stream() {
		return labels.stream();
	}

	public int size() {
		return labels.size();
	}

	public List<Label> getNeighbors(Label label, Direction direction) {
		return labels.stream().filter(l -> l != label && l.isInDirection(label, direction)).collect(Collectors.toList());
	}

	public Label getDirectNeighbor(Label label, Direction direction) {
		List<Label> neighbors = getNeighbors(label, direction);
		if (neighbors.isEmpty())
			return null;
		if (neighbors.size() == 1)
			return neighbors.get(0);
		else {
			Collections.sort(neighbors);
			switch (direction) {
				case NORTH:
				case NORTH_EAST:
				case NORTH_WEST:
				case WEST:
					return neighbors.get(neighbors.size() - 1);
				default:
					return neighbors.get(0);
			}
		}
	}

	public PagePart getPosition(Label label) {
		double xMin = Double.MAX_VALUE, yMin = Double.MAX_VALUE, xMax = 0, yMax = 0;
		for (Label l : labels) {
			GSRect rect = l.getRect();
			if (rect.getX() < xMin)
				xMin = rect.getX();
			if (rect.getY() < yMin)
				yMin = rect.getY();
			if (rect.getX() + rect.getWidth() > xMax)
				xMax = rect.getX() + rect.getWidth();
			if (rect.getY() + rect.getHeight() > yMax)
				yMax = rect.getY() + rect.getHeight();
		}
		double width = xMax - xMin;
		double height = yMax - yMin;
		GSPoint center = label.getRect().getCenter();
		int xPos, yPos;
		if (center.getX() < xMin + width / 3)
			xPos = 0;
		else if (center.getX() > xMax - width / 3)
			xPos = 2;
		else
			xPos = 1;
		if (center.getY() < yMin + height / 3)
			yPos = 0;
		else if (center.getY() > yMax - height / 3)
			yPos = 2;
		else
			yPos = 1;

		if (xPos == 0 && yPos == 0)
			return PagePart.NORTH_WEST;
		if (xPos == 0 && yPos == 1)
			return PagePart.WEST;
		if (xPos == 0 && yPos == 2)
			return PagePart.SOUTH_WEST;
		if (xPos == 1 && yPos == 0)
			return PagePart.NORTH;
		if (xPos == 1 && yPos == 1)
			return PagePart.CENTER;
		if (xPos == 1 && yPos == 2)
			return PagePart.SOUTH;
		if (xPos == 2 && yPos == 0)
			return PagePart.NORTH_EAST;
		if (xPos == 2 && yPos == 1)
			return PagePart.EAST;
		else
			return PagePart.SOUTH_EAST;
	}

	// Returns all the constraints verified by the Labels.
	// Not efficient…
	public List<Constraint> getConstraints() {
		List<Constraint> result = new ArrayList<>();
		for (Label label : this) {
			PagePart pos = getPosition(label);
			result.add(new PositionConstraint(getPosition(label), label.getText()));
			// Add RelationConstraints
			for (Direction direction : Direction.values()) {
				Label neighbour = getDirectNeighbor(label, direction);
				if (neighbour != null)
					result.add(new RelationConstraint(pos, label.getText(), direction, neighbour.getText()));
			}
		}
		// AlignmentConstraints
		for (Alignment alignment : Alignment.values()) {
			List<List<Label>> grouped = groupAlignedLabels(alignment);
			grouped.forEach(group -> {
				if (group.size() >= 2)
					result.add(new AlignmentConstraint(alignment, group.size()));
			});
		}
		return result;
	}

	public List<List<Label>> groupAlignedLabels(Alignment alignment) {
		List<Label> ll = toList();
		if (ll.isEmpty())
			return new ArrayList<>();
		List<List<Label>> result = new ArrayList<>();
		List<Label> currentGroup = new ArrayList<>(Arrays.asList(ll.get(0)));
		for (int i = 1; i < ll.size(); i++) {
			if (currentGroup.get(0).alignedWith(ll.get(i), alignment))
				currentGroup.add(ll.get(i));
			else {
				result.add(currentGroup);
				currentGroup = new ArrayList<>(Arrays.asList(ll.get(i)));
			}
		}
		result.add(currentGroup);
		return result;
	}
}
