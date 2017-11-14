package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Reinforcer2 {

	private final Logger logger = LoggerFactory.getLogger(Reinforcer2.class);
	private final List<Template2> templates = new ArrayList<>();

	public void forceTemplate(Labels labels, String templateName) {
		logger.debug("Force template {}, labels: {}", templateName, labels);
		List<Template2> matching = getMatchingTemplates(labels);
		// Nothing to do in this case.
		if (matching.size() == 1 && matching.get(0).getName().equals(templateName))
			// If only the correct result has been found:
			if (!matching.get(0).hasConstraints())
				// – if this is the second item added to the target template, find a common constraint to add to the template
				matching.get(0).addConstraintIncluding(labels);
			else
				// – otherwise return without modifying any template.
				return;

		Template2 template = templates.stream().filter(t -> t.getName().equals(templateName)).findFirst().orElse(null);

		// Add a constraint to all the matching templates that are not the correct answer so that the given item does not match with them anymore.
		matching.stream().filter(t -> !t.getName().equals(templateName)).forEach(otherTemplate -> otherTemplate.addConstraintExcluding(labels));

		if (template == null) {
			template = new Template2(templateName);
			templates.add(template);
			template.addConfirmedMember(labels);
		} else if (matching.stream().allMatch(t -> !t.getName().equals(templateName))) {
			// The correct template has not been found so it has too restrictive constraints.
			// 1. Remove constraints from target class so that it matches.
			template.removeConstraintToInclude(labels);
			// 2. Verify that the labels from the other templates are not considered as members of template.
			for (Template2 other : templates)
				for (Labels exclude : other.confirmedMembers)
					if (template.matches(exclude))
						template.addConstraintExcluding(exclude);
		}
	}

	public List<Template2> getMatchingTemplates(Labels labels) {
		return templates.stream().filter(t -> t.matches(labels)).collect(Collectors.toList());
	}

	public String getTemplate(Labels labels) {
		List<Template2> matching = getMatchingTemplates(labels);
		if (matching.isEmpty())
			throw new IllegalStateException("No template found for " + labels);
		if (matching.size() > 1)
			throw new IllegalStateException("Multiple templates found for " + labels);
		return matching.get(0).getName();
	}

	@Override
	public String toString() {
		return templates.toString();
	}
}
