package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.genericsystem.issuetracker.generalization.ManyToOne;
import org.genericsystem.issuetracker.model.Priority;
import org.genericsystem.issuetracker.qualifier.Provide;

public class PriorityBean extends ManyToOne implements Serializable {
	private static final long serialVersionUID = 3628359912273571503L;

	@Inject
	@Provide
	private transient Priority priority;

	private transient List<String> priorities;

	@Override
	@PostConstruct
	protected void initPriority() {
		priorities = priority.getInstances().stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	@Override
	public List<String> getPriorities() {
		return priorities;
	}

}
