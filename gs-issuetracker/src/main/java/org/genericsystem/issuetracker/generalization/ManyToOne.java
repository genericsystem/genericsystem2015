package org.genericsystem.issuetracker.generalization;

import java.util.List;

import javax.annotation.PostConstruct;

public abstract class ManyToOne {

	@PostConstruct
	protected abstract void initPriority();

	public abstract List<String> getPriorities();

}
