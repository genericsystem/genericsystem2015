package org.genericsystem.reactor;

//import org.genericsystem.kernel.Engine;

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
