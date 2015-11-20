package org.genericsystem.newgui.context;

import java.util.LinkedList;

import javafx.scene.Node;

public interface IModelContext {
	IModelContext getParent();

	public void registre(Node node);

	public static abstract class AbstractModelContext implements IModelContext {

		private final IModelContext parent;
		private final Object model;

		protected LinkedList<Node> nodes = new LinkedList<>();

		public AbstractModelContext(Object model, IModelContext parent) {
			this.parent = parent;
			this.model = model;

		}

		@Override
		public IModelContext getParent() {
			return parent;
		}

	}

	public static class ModelContextImpl extends AbstractModelContext {

		public ModelContextImpl(Object model, IModelContext parent) {
			super(model, parent);
		}

		@Override
		public void registre(Node node) {
			nodes.add(node);
		}
	}

}
