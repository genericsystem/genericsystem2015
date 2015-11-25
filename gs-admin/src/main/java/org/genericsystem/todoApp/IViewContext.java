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

	public List<AbstractViewContext> getChildren();

	public void destroy();

	public static abstract class AbstractViewContext implements IViewContext {
		protected Element template;
		protected Node node;
		protected IModelContext modelContext;
		protected IViewContext parent;
		protected boolean initContent = true;

		public List<AbstractViewContext> children = new ArrayList<>();

		public AbstractViewContext(IModelContext modelContext, Element template, Node node, IViewContext parent) {
			super();
			this.template = template;
			this.node = node;
			this.modelContext = modelContext;
			this.parent = parent;
		}

		@Override
		public Element getTemplate() {
			return template;
		}

		public void setTemplate(Element template) {
			this.template = template;
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

		public void setModelContext(IModelContext modelContext) {
			this.modelContext = modelContext;
		}

		@Override
		public IViewContext getParent() {
			return parent;
		}

		public void setParent(IViewContext parent) {
			this.parent = parent;
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
		public List<AbstractViewContext> getChildren() {
			return children;
		}

		@Override
		public void destroy() {
			if (node.getParent() instanceof Pane)
				((Pane) node.getParent()).getChildren().remove(node);
		}
	}

	public static class ElementViewContext extends AbstractViewContext {

		public ElementViewContext(IModelContext modelContext, Element template, Node node, IViewContext parent) {
			super(modelContext, template, node, parent);
		}

		public void initChildren() {
			if (template.content != null)
				template.content.forEach(element -> {
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
			if (template.binding != null) {
				for (int i = 0; i < template.binding.length; i++) {
					template.binding[i].init(bindingContext);
				}

			}
			if (initContent)
				initChildren();
		}

		public void registre(IViewContext viewContext) {
			modelContext.registre(viewContext);
		}

		public void addChildren(AbstractViewContext viewContext) {
			if (node instanceof Pane)
				((Pane) node).getChildren().add(viewContext.node);
			children.add(viewContext);
		}

		@Override
		public void bind(IModelContext modelContext) {
			// System.out.println("ElementViewContext::BIND")
			ElementViewContext wrapper = new ElementViewContext(modelContext, template, node, this);
			wrapper.initChildren();
		}
	}
}
