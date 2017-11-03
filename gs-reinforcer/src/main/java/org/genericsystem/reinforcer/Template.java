package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.genericsystem.reinforcer.tools.StringCompare;
import org.genericsystem.reinforcer.tools.StringCompare.SIMILARITY;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Template {

	private final Logger logger = LoggerFactory.getLogger(Template.class);
	protected final Set<Labels> contents = new HashSet<>();
	private List<LabelDesc> description = new ArrayList<>();
	private int nbExamples = 1;

	public Template(Labels labels) {
		Labels normalized = labels.normalizeLabels();
		contents.add(normalized);
		for (Label label : normalized)
			description.add(new LabelDesc(label, 1, 1));
	}

	public double getMatchRate(Labels others) {
		Labels normalized = others.normalizeLabels();
		Map<LabelDesc, List<Label>> closest = getClosestMap(normalized);
		double total = 0;
		// The number of labels in the argument equals the number of zones in description,
		// and each zone in the description corresponds to a unique label.
		if (normalized.size() == description.size() && closest.values().stream().allMatch(l -> l.size() == 1)) {
			total = closest.entrySet().stream().map(entry -> entry.getKey().matchRate(entry.getValue().get(0))).reduce(0d, Double::sum);
			total /= description.size();
		} else {
			// TODO: ?????
		}
		logger.debug("============== Template match =================");
		logger.debug("template labels: {}", description);
		logger.debug("other normalized labels: {}", normalized);
		logger.debug("matchRate: {}", total);
		logger.debug("===============================================");
		return total;
	}

	public void addLabels(Labels labels) {
		contents.add(labels);
	}

	public boolean contains(Labels al) {
		return contents.contains(al);
	}

	public void reinforce(Labels absoluteLabels) {
		nbExamples++;
		Labels normalized = absoluteLabels.normalizeLabels();
		Map<LabelDesc, List<Label>> closest = getClosestMap(normalized);
		for (Entry<LabelDesc, List<Label>> entry : closest.entrySet()) {
			if (entry.getValue().size() == 1) {
				LabelDesc key = entry.getKey();
				Label matched = entry.getValue().get(0);
				if (RectangleTools.isInCluster(matched.getRect(), key.rect, RectangleTools.DEFAULT_EPSILON))
					key.adaptText(matched.getText());
				else {
					// Assumes the labels actually correspond to the same data, which is not necessarily true.
					key.adaptPos(matched.getRect(), nbExamples);
					key.adaptText(matched.getText());
				}
			} else {
				// TODO: ???
			}
		}
	}

	// Returns the LabelDesc from this template that is closest in position to the given label.
	private LabelDesc getClosest(Label label) {
		LabelDesc closest = null;
		double minDistance = Double.MAX_VALUE;
		for (LabelDesc desc : description) {
			double dist = desc.getRect().diffWith(label.getRect(), RectangleTools.DEFAULT_EPSILON);
			if (dist < minDistance) {
				minDistance = dist;
				closest = desc;
			}
		}
		return closest;
	}

	private Map<LabelDesc, List<Label>> getClosestMap(Labels normalized) {
		Map<LabelDesc, List<Label>> closest = new HashMap<>();
		for (Label label : normalized) {
			LabelDesc cl = getClosest(label);
			if (closest.get(cl) == null)
				closest.put(cl, Arrays.asList(label));
			else
				closest.get(cl).add(label);
		}
		return closest;
	}

	public static class LabelDesc {
		private final Logger logger = LoggerFactory.getLogger(LabelDesc.class);
		// TODO: Allow definition of left-aligned, right-aligned or centered areas.
		private GSRect rect;
		private String text;
		// weight between 0 and 1
		private double posWeight;
		private double textWeight;

		public LabelDesc(Label label, double posWeight, double textWeight) {
			rect = label.getRect();
			text = label.getText();
			this.posWeight = posWeight;
			this.textWeight = textWeight;
		}

		public double matchRate(Label label) {
			double result;
			if (text != null)
				result = 1 - (posWeight * rect.diffWith(label.getRect(), RectangleTools.DEFAULT_EPSILON)
						+ textWeight * (1 - StringCompare.compare(text, label.getText(), StringCompare.SIMILARITY.LEVENSHTEIN))) / 2;
			else
				result = 1 - posWeight * rect.diffWith(label.getRect(), RectangleTools.DEFAULT_EPSILON);
			logger.debug("matchrate between label {} and labelDesc {}: {}.", label, this, result);
			return result;
		}

		public void adaptText(String otherText) {
			double levSim = StringCompare.compare(text, otherText, SIMILARITY.LEVENSHTEIN);
			if (levSim < 0.1) {
				// Two class elements with completely different texts so the text is ignored.
				text = null;
				textWeight = 0;
			} else {
				// TODO
			}
		}

		public void adaptPos(GSRect otherPos, int nbExamples) {
			rect = RectangleTools.linearCombination(rect, (nbExamples - 1.0) / nbExamples, otherPos, 1.0 / nbExamples);
		}

		public GSRect getRect() {
			return rect;
		}

		@Override
		public String toString() {
			return "{LabelDesc " + rect.toString() + ", content: " + text + ", posWeight: " + posWeight + ", textWeight: " + textWeight + "}";
		}
	}
}
