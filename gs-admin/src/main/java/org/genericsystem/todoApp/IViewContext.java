package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.genericsystem.todoApp.IElement.AbstractElement;
import org.genericsystem.todoApp.IElement.Element;
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
					try {
						childNode = createNode(((AbstractElement) element).classNode);
						registre(childNode);
					} catch (Exception e) {
						throw new IllegalStateException(e);
					}

					Element elm = new Element(((AbstractElement) element).classNode, ((AbstractElement) element).text.get(), ((AbstractElement) element).binding, ((AbstractElement) element).content);
					ElementViewContext viewContextChild = new ElementViewContext(modelContext, elm, childNode, this);
					addChildren(viewContextChild);
					viewContextChild.init();
				});
			System.out.println();
		}

		private Node createNode(Class<? extends Node> clazz) throws InstantiationException, IllegalAccessException {
			return clazz.newInstance();
		}

		public void init() {
			BindingContext bindingContext = new BindingContext(modelContext, this);
			if (template.binding != null) {
				template.binding.init(bindingContext);
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
