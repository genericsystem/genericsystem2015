package org.genericsystem.reinforcer;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.functions.Consumer;
import io.reactivex.subjects.PublishSubject;

public class Reinforcer {

	private static final Logger logger = LoggerFactory.getLogger(Reinforcer.class);
	private static final double MATCHING_RATE = 0.8;
	private final Map<String, Template> templates = new HashMap<>();
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

	public void reinforce(Labels labels, String templateName) {
		Template template = templates.get(templateName);
		if (template != null)
			template.reinforce(labels);
		else
			logger.warn("No template of name {} found.", templateName);
	}

	public void createNewTemplate(Labels labels, String templateName) {
		if (templates.containsKey(templateName))
			throw new IllegalStateException("A template with name " + templateName + " already exists.");
		Template template = new Template(labels);
		templates.put(templateName, template);
	}

	public String getBestTemplate(Labels labels) {
		double bestMatchRate = 0;
		String bestTemplate = null;
		for (Entry<String, Template> entry : templates.entrySet()) {
			logger.info("Testing template " + entry.getKey());
			double matchRate = entry.getValue().getMatchRate(labels);
			if (matchRate > bestMatchRate) {
				bestMatchRate = matchRate;
				bestTemplate = entry.getKey();
			}
		}
		if (bestMatchRate > MATCHING_RATE) {
			logger.info("Best template found: {}, labels: {}", bestTemplate, labels);
			return bestTemplate;
		} else {
			logger.info("No template found, labels: {}", labels);
			return null;
		}
	}

	public void reinforce(Labels absoluteLabels) {
		double bestMatchRate = 0;
		Template bestTemplate = null;
		for (Template template : templates.values()) {
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
