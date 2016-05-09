package org.genericsystem.distributed.ui;

import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class Model {

	Model parent;
	Element<?, ?> element;

	public Model getParent() {
		return parent;
	}

	public Element<?, ?> getElement() {
		assert element != null;
		return element;
	}

	public void afterParentConstruct() {

	}

	public static class EngineModel extends Model {

		public Engine getEngine() {
			return ((EngineModel) getParent()).getEngine();
		}

	}
}
