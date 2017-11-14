package org.genericsystem.reinforcer;

import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;

public interface Constraint {

	// Returns true iff the given labels verify the constraint.
	boolean check(Labels labels);

	// Verified if the labels contains a label with the correct text in the given page part.
	public class PositionConstraint implements Constraint {
		protected PagePart pagePart;
		protected String contents;

		public PositionConstraint(PagePart pagePart, String contents) {
			this.pagePart = pagePart;
			this.contents = contents;
		}

		@Override
		public boolean check(Labels labels) {
			return labels.stream().anyMatch(label -> labels.getPosition(label) == pagePart && label.matchesText(contents));
		}

		public List<Label> getMatchingLabels(Labels labels) {
			return labels.stream().filter(label -> labels.getPosition(label) == pagePart && label.matchesText(contents)).collect(Collectors.toList());
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == this)
				return true;
			if (obj == null || getClass() != obj.getClass())
				return false;
			PositionConstraint other = (PositionConstraint) obj;
			return pagePart == other.pagePart && StringCompare.similar(contents, other.contents, SIMILARITY.LEVENSHTEIN);
		}

		@Override
		public int hashCode() {
			return pagePart.hashCode();
		}

		@Override
		public String toString() {
			return "pagePart: " + pagePart + ", contents: " + contents;
		}
	}

	public class RelationConstraint extends PositionConstraint {

		private Direction direction;
		private String targetContents;

		public RelationConstraint(PagePart pagePart, String sourceContents, Direction direction, String targetContents) {
			super(pagePart, sourceContents);
			this.direction = direction;
			this.targetContents = targetContents;
		}

		public RelationConstraint(PagePart pagePart, String sourceContents, Direction direction) {
			super(pagePart, sourceContents);
			this.direction = direction;
		}

		@Override
		public boolean check(Labels labels) {
			List<Label> sources = super.getMatchingLabels(labels);
			if (sources.isEmpty())
				return false;
			for (Label source : sources) {
				List<Label> possibleTargets = labels.getNeighbors(source, direction);
				if (targetContents == null)
					return !possibleTargets.isEmpty();
				for (Label target : possibleTargets)
					if (target.matchesText(targetContents))
						return true;
			}
			return false;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == this)
				return true;
			if (obj == null || getClass() != obj.getClass())
				return false;
			RelationConstraint other = (RelationConstraint) obj;
			return pagePart == other.pagePart && StringCompare.similar(contents, other.contents, SIMILARITY.LEVENSHTEIN)
					&& (targetContents == null && other.targetContents == null || StringCompare.similar(targetContents, other.targetContents, SIMILARITY.LEVENSHTEIN));
		}

		@Override
		public String toString() {
			return super.toString() + ", direction: " + direction + (targetContents == null ? "" : ", target: " + targetContents);
		}
	}

	// True if some labels are aligned as indicated.
	// Used only if the other types of constraints can’t be used.
	public class AlignmentConstraint implements Constraint {

		private final Alignment alignment;
		private int min; // >= 2

		public AlignmentConstraint(Alignment alignment, int min) {
			this.alignment = alignment;
			this.min = min;
		}

		@Override
		public boolean check(Labels labels) {
			List<List<Label>> grouped = labels.groupAlignedLabels(alignment);
			return grouped.stream().anyMatch(group -> group.size() >= min);
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((alignment == null) ? 0 : alignment.hashCode());
			result = prime * result + min;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null || getClass() != obj.getClass())
				return false;
			AlignmentConstraint other = (AlignmentConstraint) obj;
			return alignment == other.alignment && min == other.min;
		}

		@Override
		public String toString() {
			return "alignment: " + alignment + ", min: " + min;
		}
	}
}
