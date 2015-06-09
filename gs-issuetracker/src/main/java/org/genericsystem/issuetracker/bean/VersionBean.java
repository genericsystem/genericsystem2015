package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.genericsystem.issuetracker.model.Version;
import org.genericsystem.issuetracker.qualifier.Provide;

public class VersionBean implements Serializable {
	private static final long serialVersionUID = -3550870042926403966L;

	@Inject
	@Provide
	private transient Version version;

	private transient List<String> versions;

	@PostConstruct
	private void init() {
		versions = version.getInstances().stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	public List<String> getVersions() {
		return versions;
	}

}
