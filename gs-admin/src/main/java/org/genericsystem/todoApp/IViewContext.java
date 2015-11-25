package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;
import org.genericsystem.todoApp.IElement.Element;
import org.genericsystem.todoApp.binding.BindingContext;

public interface IViewContext {

	void bind(IModelContext modelContext);

	public Element getTemplate();

	public Node getNode();

	public void setNode(Node node);

	public IModelContext getModelContext();

	public IViewContext getParent();

	public boolean isInitContent();

	public void setInitContent(boolean initContent);

	public List<ElementViewContext> getChildren();

	public void destroy();

	public static class ElementViewContext implements IViewContext {
		protected Element template;
		protected Node node;
		protected IModelContext modelContext;
		protected IViewContext parent;
		protected boolean initContent = true;

		public List<ElementViewContext> children = new ArrayList<>();

		public ElementViewContext(IModelContext modelContext, Element template, Node node, IViewContext parent) {
			this.template = template;
			this.node = node;
			this.modelContext = modelContext;
			this.parent = parent;
		}

		@Override
		public Element getTemplate() {
			return template;
		}

		@Override
		public Node getNode() {
			return node;
		}

		@Override
		public void setNode(Node node) {
			this.node = node;
		}

		@Override
		public IModelContext getModelContext() {
			return modelContext;
		}

		@Override
		public IViewContext getParent() {
			return parent;
		}

		@Override
		public boolean isInitContent() {
			return initContent;
		}

		@Override
		public void setInitContent(boolean initContent) {
			this.initContent = initContent;
		}

		@Override
		public List<ElementViewContext> getChildren() {
			return children;
		}

		@Override
		public void destroy() {
			if (node.getParent() instanceof Pane)
				((Pane) node.getParent()).getChildren().remove(node);
		}

		public void initChildren() {

			template.getChildren().forEach(element -> {
				Node childNode = null;
				childNode = createNode(((Element) element).classNode);

				if (childNode instanceof Button)
					((Button) childNode).setText(((Element) element).text.get());

				ElementViewContext viewContextChild = new ElementViewContext(modelContext, ((Element) element), childNode, this);
				addChildren(viewContextChild);
				registre(viewContextChild);
				viewContextChild.init();
			});
		}

		private Node createNode(Class<? extends Node> clazz) {
			try {
				return clazz.newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}

		public void init() {
			BindingContext bindingContext = new BindingContext(modelContext, this);
			for (int i = 0; i < template.binding.length; i++)
				template.binding[i].init(bindingContext);
			if (initContent)
				initChildren();
		}

		public void registre(IViewContext viewContext) {
			modelContext.registre(viewContext);
		}

		public void addChildren(ElementViewContext viewContext) {
			if (node instanceof Pane)
				((Pane) node).getChildren().add(viewContext.node);
			children.add(viewContext);
		}

		@Override
		public void bind(IModelContext modelContext) {
			ElementViewContext wrapper = new ElementViewContext(modelContext, template, node, this);
			wrapper.initChildren();
		}
	}

}
