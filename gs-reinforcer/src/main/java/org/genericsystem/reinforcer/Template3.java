package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Template3 {

	private final Logger logger = LoggerFactory.getLogger(Template3.class);
	private final String name;
	protected final List<Labels> members = new ArrayList<>();
	private List<Constraint> constraints = new ArrayList<>();
	private List<LabelDesc> description = new ArrayList<>();

	public Template3(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public boolean matches(Labels entry) {
		return constraints.stream().allMatch(constraint -> constraint.check(entry));
	}

	public void addLabels(Labels labels) {
		members.add(labels);
	}

	public boolean contains(Labels al) {
		return members.contains(al);
	}

	// Builds template from 2 labels.
	private void buildTemplate() {
		if (members.size() < 2)
			throw new IllegalStateException("There must be at least two examples for the template to be built.");

		Labels labels1 = members.get(0);
		Labels labels2 = members.get(1);
		List<Match> bestAligned = labels2.alignWith(labels1);

		for (Match match : bestAligned)
			if (match.source != null && match.match != null && StringCompare.similar(match.source.getText(), match.match.getText(), SIMILARITY.LEVENSHTEIN))
				description.add(new LabelDesc(match.source));

		description.forEach(ld -> ld.setDirection(labels2.contentDirection(ld.getLabel(), description)));
	}

	// Specifies the template when a new item is added.
	private void reinforceTemplate(Labels absoluteLabels) {
		Iterator<LabelDesc> it = description.iterator();
		while (it.hasNext()) {
			LabelDesc descriptor = it.next();
			if (!absoluteLabels.toList().stream().map(label -> label.getText()).anyMatch(c -> StringCompare.similar(c, descriptor.getLabel().getText(), SIMILARITY.LEVENSHTEIN)))
				it.remove();
		}

	}

	public List<DetectedContent> extractData(Labels labels) {
		Labels ref = new Labels();
		description.forEach(ld -> ref.addLabel(ld.getLabel()));
		List<Match> matched = ref.alignWith(labels);
		Labels aligned = new Labels();
		matched.forEach(match -> {
			if (match.match != null)
				aligned.addLabel(match.match);
		});
		List<DetectedContent> result = new ArrayList<>();
		for (Match match : matched)
			if (match.source != null && match.match != null) {
				LabelDesc ld = description.stream().filter(ref.getTest.apply(match.source)).findFirst().orElse(null);
				if (ld != null) {
					Label target = aligned.getDirectNeighbor(match.match, ld.direction);
					if (target != null)
						result.add(new DetectedContent(match.source.getText(), target.getText()));
				}
			}
		return result;
	}

	public void reinforce(Labels absoluteLabels) {
		members.add(absoluteLabels);
		if (members.size() == 2)
			buildTemplate();
		if (members.size() > 2)
			reinforceTemplate(absoluteLabels);
	}

	@Override
	public String toString() {
		return "\nTemplate " + name + "\nConstraints: " + constraints;
	}

	public static class Match {
		Label source;
		Label match;

		public Match(Label source, Label match) {
			this.source = source;
			this.match = match;
		}

		@Override
		public String toString() {
			return "\n{ Match between " + source + " and " + match + " }";
		}
	}

	public static class LabelDesc {
		private Label label;
		// Direction to look in for corresponding content.
		private Direction direction;

		public LabelDesc(Label label) {
			this.label = label;
		}

		public void setDirection(Direction direction) {
			this.direction = direction;
		}

		public Label getLabel() {
			return label;
		}

		@Override
		public String toString() {
			return "{LabelDesc " + label.toString() + ", content direction: " + direction + "}";
		}
	}
}
