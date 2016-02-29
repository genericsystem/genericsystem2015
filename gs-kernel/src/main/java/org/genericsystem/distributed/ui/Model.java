package org.genericsystem.distributed.ui;

public abstract class Model {

	Model parent;

	public Model getParent() {
		return parent;
	}

	public void afterParentConstruct() {

	}

}
