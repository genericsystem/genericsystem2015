package org.genericsystem.issuetracker.bean;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.cdi.Engine;

@Named
@RequestScoped
public class CacheBean {

	@Inject
	private Engine engine;

	public String flush() {
		engine.getCurrentCache().flush();
		return "#";
	}

	public String clear() {
		engine.getCurrentCache().clear();
		return "#";
	}

}
