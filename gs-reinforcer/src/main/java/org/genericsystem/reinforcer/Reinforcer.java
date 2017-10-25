package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.reinforcer.Template;
import org.genericsystem.reinforcer.Unclassifiable;

import io.reactivex.functions.Consumer;
import io.reactivex.subjects.PublishSubject;

public class Reinforcer {

	private static final double MATCHING_RATE = 0.9;
	private final List<Template> templates = new ArrayList<>();
	private final Unclassifiable unclassifiable = new Unclassifiable();

	public static void main(String[] args) {
		Reinforcer reinforcer = new Reinforcer();
		PublishSubject<Labels> source = PublishSubject.create();
		source.subscribe(reinforcer.getObserver());
		Labels labels = new Labels();
		labels.addLabel(0, 0, 10, 10, "First Label");
		labels.addLabel(5, 5, 15, 15, "Second Label");

		source.onNext(labels);

		// reinforcer.reinforce(new AbsoluteLabels().addAbsoluteLabel(0, 0, 10, 10, "First Label").addAbsoluteLabel(5, 5, 15, 15, "Second Label"));

	}

	public Consumer<Labels> getObserver() {
		return labels -> reinforce(labels);
	}

	public void reinforce(Labels absoluteLabels) {
		double bestMatchRate = 0;
		Template bestTemplate = null;
		for (Template template : templates) {
			double matchRate = template.getMatchRate(absoluteLabels);
			if (matchRate > bestMatchRate) {
				bestMatchRate = matchRate;
				bestTemplate = template;
			}
		}
		if (bestMatchRate > MATCHING_RATE)
			bestTemplate.reinforce(absoluteLabels);
		unclassifiable.reinforce(absoluteLabels);
	}

	public Unclassifiable getUnclassifiable() {
		return unclassifiable;
	}

}
