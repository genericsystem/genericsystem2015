package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.genericsystem.issuetracker.model.Statut;
import org.genericsystem.issuetracker.qualifier.Provide;

public class StatutBean implements Serializable {

	private static final long serialVersionUID = -7612611706483937791L;

	@Inject
	@Provide
	private transient Statut statut;
	private transient List<String> statuts;

	@PostConstruct
	private void init() {
		statuts = statut.getInstances().stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	public List<String> getStatuts() {
		return statuts;
	}

}
