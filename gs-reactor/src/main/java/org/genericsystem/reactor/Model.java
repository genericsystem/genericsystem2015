package org.genericsystem.reactor;

//import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class Model {

	Model parent;
	ModelContext modelContext;

	public Model getParent() {
		return parent;
	}
	
	public ModelContext getModelContext() {
		return modelContext;
	}

	public void afterParentConstruct() {

	}
}
