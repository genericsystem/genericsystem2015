package org.genericsystem.ui;

public abstract class Model {
	private final Model parent;

	public Model(Model parent) {
		this.parent = parent;
	}

	public Model getParent() {
		return parent;
	}

}
