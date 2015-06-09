package org.genericsystem.examplejsf.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.cdi.Engine;
import org.genericsystem.examplejsf.model.Color;
import org.genericsystem.mutability.Generic;

@Named
@SessionScoped
public class ColorBean implements Serializable {

	private static final long serialVersionUID = 9201611589820526869L;

	@Inject
	private transient Engine engine;
	private transient Generic color;
	private transient List<String> colors;

	@PostConstruct
	private void init() {
		color = engine.find(Color.class);
		colors = color.getInstances().stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	public List<String> getColors() {
		return colors;
	}

}
