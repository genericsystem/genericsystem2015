package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.List;

public class Extractor {

	// Receives labels together with their class. Retrieves appropriate data.
	private final List<Template3> templates = new ArrayList<>();

	public void reinforce(Labels labels, String templateName) {
		Template3 template = templates.stream().filter(t -> t.getName().equals(templateName)).findFirst().orElse(null);
		if (template == null) {
			template = new Template3(templateName);
			templates.add(template);
		}
		template.reinforce(labels);
	}

	public List<DetectedContent> extractData(Labels labels, String templateName) {
		Template3 template = templates.stream().filter(t -> t.getName().equals(templateName)).findFirst().orElse(null);
		if (template == null)
			throw new IllegalStateException("Inexistent template " + templateName);
		return template.extractData(labels);
	}

	@Override
	public String toString() {
		return templates.toString();
	}
}
