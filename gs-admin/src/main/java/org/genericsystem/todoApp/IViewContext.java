package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;

import org.genericsystem.todoApp.IElement.AbstractElement;
import org.genericsystem.todoApp.binding.BindingContext;

public interface IViewContext {

	void bind(IModelContext modelContext);

	public static abstract class AbstractViewContext implements IViewContext {
		public AbstractElement template;
		public Node node;
		public IModelContext modelContext;
		public IViewContext parent;
		public boolean initContent = true;

		public List<AbstractViewContext> children = new ArrayList<>();

		public AbstractViewContext(IModelContext modelContext, AbstractElement template, Node node, IViewContext parent) {
			super();
			this.template = template;
			this.node = node;
			this.modelContext = modelContext;
			this.parent = parent;
		}
	}

	public static class ElementViewContext extends AbstractViewContext {

		public ElementViewContext(IModelContext modelContext, AbstractElement template, Node node, IViewContext parent) {
			super(modelContext, template, node, parent);
		}

		public void initChildren() {
			if (template.content != null)
				template.content.forEach(element -> {
					Node childNode = null;
					childNode = createNode(((AbstractElement) element).classNode);
					registre(childNode);
					if (childNode instanceof Button)
						((Button) childNode).setText(((AbstractElement) element).text.get());

					ElementViewContext viewContextChild = new ElementViewContext(modelContext, ((AbstractElement) element), childNode, this);
					addChildren(viewContextChild);
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

		public void registre(Node node) {
			modelContext.registre(node);
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
