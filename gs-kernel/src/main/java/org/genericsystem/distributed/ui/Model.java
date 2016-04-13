package org.genericsystem.distributed.ui;

import org.genericsystem.kernel.Engine;

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

	public static class EngineModel extends Model {

		public Engine getEngine() {
			return ((EngineModel) getParent()).getEngine();
		}

	}
}
