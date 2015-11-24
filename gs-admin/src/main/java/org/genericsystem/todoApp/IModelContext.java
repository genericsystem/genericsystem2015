package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

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
			this.nodes.add(node);
		}

		@Override
		public void destroy() {
			nodes.forEach(e -> {
				if (e.getParent() instanceof Pane)
					((Pane) e.getParent()).getChildren().remove(e);
			});
		}

		public AbstractModelContext resolve(Method method) {
			if (method.getDeclaringClass().isAssignableFrom(this.model.getClass()))
				return this;
			else if (this.parent == null)
				throw new IllegalStateException("Unable to resolve method : " + method);
			else
				return ((AbstractModelContext) parent).resolve(method);
		}

		public AbstractModelContext resolve(Field field) {
			if (field.getDeclaringClass().isAssignableFrom(this.model.getClass()))
				return this;
			else if (this.parent == null)
				throw new IllegalStateException("Unable to resolve field : " + field);
			else
				return ((AbstractModelContext) parent).resolve(field);
		}
	}

	public static class ModelContextImpl extends AbstractModelContext {
		public ModelContextImpl(IModelContext parent, Object model) {
			super(parent, model);
		}

		@Override
		public ModelContextImpl createChild(Object child) {
			return new ModelContextImpl(this, child);
		}
	}

	public void destroy();
}
