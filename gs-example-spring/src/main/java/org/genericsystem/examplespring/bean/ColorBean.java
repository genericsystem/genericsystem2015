package org.genericsystem.examplespring.bean;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.genericsystem.common.Generic;
import org.genericsystem.models.Color;
import org.genericsystem.spring.Engine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("session")
public class ColorBean implements Serializable {

	private static final long serialVersionUID = 9201611589820526869L;

	@Autowired
	private transient Engine engine;
	private transient Generic color;
	private transient Map<String, String> colors;

	@PostConstruct
	private void init() {
		color = engine.find(Color.class);
		colors = color.getInstances().stream()
				.collect(Collectors.toMap(generic -> Objects.toString(generic.getValue()), generic -> Objects.toString(generic.getValue())));
	}

	public Map<String, String> getColors() {
		return colors;
	}

}
