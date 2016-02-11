package org.genericsystem.distributed.cacheonserver.ui.js;

public abstract class Model {

	Model parent;

	public Model getParent() {
		return parent;
	}

	public void afterParentConstruct() {

	}

}
