package org.genericsystem.distributed.ui;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class Model {

	Model parent;

	public Model getParent() {
		return parent;
	}

	public void afterParentConstruct() {

	}
}
