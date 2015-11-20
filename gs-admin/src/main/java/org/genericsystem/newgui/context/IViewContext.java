package org.genericsystem.newgui.context;

import javafx.scene.Node;

import org.genericsystem.newgui.component.Element.AbstractElement;
import org.genericsystem.newgui.context.IModelContext.ModelContextImpl;

public interface IViewContext {
	public Node getNode();

	public void bind(IModelContext modelContext);

	public static abstract class AbstractViewContext implements IViewContext {

		private final Node node;
		private final ModelContextImpl modelContext;
		private final AbstractElement element;

		public AbstractViewContext(ModelContextImpl modelContext, Node node, AbstractElement element) {
			this.node = node;
			this.modelContext = modelContext;
			this.element = element;
		}

		@Override
		public Node getNode() {
			return this.node;
		}
	}

	public static class ElementViewContext extends AbstractViewContext {

		public ElementViewContext(ModelContextImpl modelContext, Node node, AbstractElement thisElement) {
			super(modelContext, node, thisElement);
		}

		@Override
		public void bind(IModelContext modelContext) {

		}

		public void init() {

		}

	}
}
