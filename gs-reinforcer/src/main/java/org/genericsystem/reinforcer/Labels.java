package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.reinforcer.Constraint.ColsConstraint;
import org.genericsystem.reinforcer.Constraint.PositionConstraint;
import org.genericsystem.reinforcer.Constraint.RelationConstraint;
import org.genericsystem.reinforcer.Template3.LabelDesc;
import org.genericsystem.reinforcer.Template3.Match;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;

public class Labels implements Iterable<Label> {

	private final Set<Label> labels = new HashSet<>();
	private static final double THRESHOLD = 0.1; // Error threshold for the computation of the affine transformation in alignWith.

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

	// Assumes that the other item belongs to the same class as this, and transforms this
	// so matching fields have the same coordinates.
	public List<Match> alignWith(Labels item) {
		List<Match> matches = new ArrayList<>();

		for (Label label : this)
			for (Label other : item)
				if (StringCompare.similar(label.getText(), other.getText(), SIMILARITY.LEVENSHTEIN))
					matches.add(new Match(label, other));

		if (matches.isEmpty())
			return new ArrayList<>();

		MatchListWithRate bestMatches = null;

		Set<Integer> tested = new HashSet<>();
		for (int i = 0; i < matches.size(); i++) {
			if (tested.add(i)) {
				Set<Match> selectedMatches = new HashSet<>();
				selectedMatches.add(matches.get(i));
				AffineTransformation possibleTransformation = new AffineTransformation(selectedMatches);

				for (int j = 0; j < matches.size(); j++)
					if (j != i && !tested.contains(j) && possibleTransformation.computeError(matches.get(j)) < THRESHOLD) {
						selectedMatches.add(matches.get(j));
						tested.add(j);
					}

				possibleTransformation = new AffineTransformation(selectedMatches);
				Labels aligned = possibleTransformation.transform(this);
				MatchListWithRate result = aligned.matchRate(item);
				if (bestMatches == null || result.matchRate > bestMatches.matchRate) {
					bestMatches = result;
					if (bestMatches.matchRate == 1)
						break;
				}
			}
		}
		return bestMatches.matchList;
	}

	private Comparator<Label> labelComparator = (l1, l2) -> {
		GSRect r1 = l1.getRect();
		GSRect r2 = l2.getRect();
		if (r1.getY() != r2.getY())
			return Double.compare(r1.getY(), r2.getY());
		return Double.compare(r1.getX(), r2.getX());
	};

	Function<Label, Predicate<LabelDesc>> getTest = label -> ld -> ld.getLabel() == label;

	// Compute the direction to look in to get the content associated with a given label.
	public Direction contentDirection(Label label, List<LabelDesc> description) {
		Label neighbor = getDirectNeighbor(label, Direction.EAST);
		if (neighbor != null && !description.stream().anyMatch(getTest.apply(neighbor)))
			return Direction.EAST;
		neighbor = getDirectNeighbor(label, Direction.SOUTH);
		if (neighbor != null && !description.stream().anyMatch(getTest.apply(neighbor)))
			return Direction.SOUTH;
		else
			throw new IllegalStateException("Impossible to detect content direction for " + label + " among " + this);
	}

	private MatchListWithRate matchRate(Labels others) {
		int m = size();
		int n = others.size();
		List<Label> l1 = toList();
		Collections.sort(l1, labelComparator);
		List<Label> l2 = others.toList();
		Collections.sort(l2, labelComparator);
		Object[] costsAndSteps = computeCostsAndSteps(l1, l2, m, n);
		Step[][] steps = (Step[][]) costsAndSteps[1];
		List<Match> bestMatch = computeBestMatch(l1, l2, steps, m, n);

		double[][] costs = (double[][]) costsAndSteps[0];
		double total = costs[m][n];
		total /= others.size();
		total = 1 - total;
		return new MatchListWithRate(bestMatch, total);
	}

	// Should return a pair (double[][], Step[][]).
	private Object[] computeCostsAndSteps(List<Label> source, List<Label> target, int m, int n) {
		// Initialization
		double[][] costs = new double[m + 1][n + 1];
		costs[0][0] = 0;
		for (int i = 1; i <= m; i++)
			costs[i][0] = costs[i - 1][0] + insertionCost(source.get(i - 1));
		for (int j = 1; j <= n; j++)
			costs[0][j] = costs[0][j - 1] + insertionCost(target.get(j - 1));

		// Recursion
		Step[][] steps = new Step[m + 1][n + 1];
		steps[0][0] = Step.NONE;
		for (int i = 1; i <= m; i++)
			steps[i][0] = Step.INSERTION;
		for (int j = 1; j <= n; j++)
			steps[0][j] = Step.DELETION;
		for (int i = 1; i <= m; i++)
			for (int j = 1; j <= n; j++) {
				double costInsertion = costs[i - 1][j] + insertionCost(source.get(i - 1));
				double costNoChange = costs[i - 1][j - 1] + source.get(i - 1).alignmentCost(target.get(j - 1));
				double costDeletion = costs[i][j - 1] + insertionCost(target.get(j - 1));
				costs[i][j] = Math.min(costNoChange, Math.min(costInsertion, costDeletion));
				if (costs[i][j] == costNoChange)
					steps[i][j] = Step.NONE;
				else if (costs[i][j] == costInsertion)
					steps[i][j] = Step.INSERTION;
				else
					steps[i][j] = Step.DELETION;
			}

		Object[] result = new Object[] { costs, steps };
		return result;
	}

	// source and target must be sorted lists.
	private List<Match> computeBestMatch(List<Label> source, List<Label> target, Step[][] steps, int i, int j) {
		if (i == 0 && j == 0)
			return new ArrayList<>();

		List<Match> bestMatch;
		switch (steps[i][j]) {
			case NONE:
				bestMatch = computeBestMatch(source, target, steps, i - 1, j - 1);
				bestMatch.add(new Match(source.get(i - 1), target.get(j - 1)));
				break;
			case INSERTION:
				bestMatch = computeBestMatch(source, target, steps, i - 1, j);
				bestMatch.add(new Match(source.get(i - 1), null));
				break;
			case DELETION:
			default:
				bestMatch = computeBestMatch(source, target, steps, i, j - 1);
				bestMatch.add(new Match(null, target.get(j - 1)));
				break;
		}

		return bestMatch;
	}

	private double insertionCost(Label label) {
		return 1;
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
		// AlignmentConstraint
		// result.add(new AlignmentConstraint(this));
		result.add(new ColsConstraint(this));
		return result;
	}

	// Groups the given labels so that those that are considered equal with the given function are together.
	// Assumes that the given labels’ list is sorted so that labels to be grouped are adjacent each other.
	private List<List<Label>> groupBy(List<Label> labels, BiFunction<Label, Label, Boolean> equals) {
		if (labels.isEmpty())
			return new ArrayList<>();
		List<List<Label>> result = new ArrayList<>();
		List<Label> currentGroup = new ArrayList<>(Arrays.asList(labels.get(0)));
		for (int i = 1; i < labels.size(); i++) {
			if (equals.apply(currentGroup.get(0), labels.get(i)))
				currentGroup.add(labels.get(i));
			else {
				result.add(currentGroup);
				currentGroup = new ArrayList<>(Arrays.asList(labels.get(i)));
			}
		}
		result.add(currentGroup);
		return result;
	}

	public List<List<Label>> groupAlignedLabels(Alignment alignment) {
		List<Label> ll = toList();
		Collections.sort(ll, (l1, l2) -> {
			if (alignment == Alignment.LEFT)
				return Double.compare(l1.getRect().getX(), l2.getRect().getX());
			else
				return Double.compare(l1.getRect().br().getX(), l2.getRect().br().getX());
		});
		return groupBy(ll, (l1, l2) -> l1.alignedWith(l2, alignment));
	}

	private List<List<Label>> byLine(List<Label> labels) {
		Collections.sort(labels);
		return groupBy(labels, (l1, l2) -> {
			GSPoint c1 = l1.getRect().getCenter();
			GSPoint c2 = l2.getRect().getCenter();
			return Math.abs(c1.getY() - c2.getY()) <= .1 * Math.max(c1.getY(), c2.getY());
		});
	}

	public List<List<Label>> groupByLine() {
		return byLine(toList());
	}

	// Returns the labels in the line above or below label’s line that overlap horizontally with it.
	private List<Label> computeOverlapping(Label label, List<List<Label>> lines) {
		int lineNo = 0;
		for (int i = 0; i <= lines.size(); i++)
			if (lines.get(i).contains(label)) {
				lineNo = i;
				break;
			}
		List<Label> overlaps = new ArrayList<>();
		if (lineNo > 0)
			for (Label other : lines.get(lineNo - 1))
				if (label.getRect().hOverlaps(other.getRect()))
					overlaps.add(other);
		if (lineNo < lines.size() - 1)
			for (Label other : lines.get(lineNo + 1))
				if (label.getRect().hOverlaps(other.getRect()))
					overlaps.add(other);
		return overlaps;
	}

	// Returns the labels that belong to the same block as the given label.
	private List<Label> expandBlock(Label label, List<Label> block, List<List<Label>> lines, Set<Label> expanded) {
		expanded.add(label);
		block.add(label);
		for (Label other : computeOverlapping(label, lines))
			if (expanded.add(other))
				expandBlock(other, block, lines, expanded);
		return block;
	}

	private List<List<Label>> findBlocks() {
		Set<Label> expanded = new HashSet<>();
		List<List<Label>> lines = groupByLine();
		List<List<Label>> blocks = new ArrayList<>();
		for (Label label : this)
			if (!expanded.contains(label))
				blocks.add(expandBlock(label, new ArrayList<>(), lines, expanded));
		return blocks;
	}

	// Returns blocks that contain only one label per line.
	public List<List<Label>> findCols() {
		List<List<Label>> blocks = findBlocks();
		return blocks.stream().filter(block -> {
			List<List<Label>> lines = byLine(block);
			return lines.size() > 1 && lines.stream().allMatch(line -> line.size() == 1);
		}).collect(Collectors.toList());
	}

	private enum Step {
		INSERTION,
		DELETION,
		NONE,
	}

	public static class MatchListWithRate {
		protected final List<Match> matchList;
		protected final double matchRate;

		MatchListWithRate(List<Match> matchList, double matchRate) {
			this.matchList = matchList;
			this.matchRate = matchRate;
		}

		@Override
		public String toString() {
			return matchList + ", matchRate: " + matchRate;
		}
	}
}
