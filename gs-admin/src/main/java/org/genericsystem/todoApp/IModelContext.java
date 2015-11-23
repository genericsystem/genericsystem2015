package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;

public interface IModelContext {

	public IModelContext createChild(Object childModel);

	void registre(Node node);

	public static abstract class AbstractModelContext implements IModelContext {

		List<Node> nodes = new ArrayList<>();
		public IModelContext parent;
		public Object model;

		public AbstractModelContext(IModelContext parent, Object model) {
			super();
			this.parent = parent;
			this.model = model;
		}

		@Override
		public void registre(Node node) {
			// System.out.println("ModelContextImpl::registre(" + node + ")");
			this.nodes.add(node);
		}
	}

	public static class ModelContextImpl extends AbstractModelContext {
		public ModelContextImpl(IModelContext parent, Object model) {
			super(parent, model);
		}

		@Override
		public ModelContextImpl createChild(Object child) {
			// System.out.println("ModelContextImpl::create child");
			// System.out.println("############### :: " + child.getClass());
			return new ModelContextImpl(this, child);
		}
	}
}
