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

		public List<AbstractViewContext> children = new ArrayList<>();

		public AbstractViewContext(IModelContext modelContext, AbstractElement template, Node node) {
			super();
			this.template = template;
			this.node = node;
			this.modelContext = modelContext;
		}
	}

	public static class ElementViewContext extends AbstractViewContext {

		public ElementViewContext(IModelContext modelContext, AbstractElement template, Node node) {
			super(modelContext, template, node);
			System.out.println("ElementViewContext:: " + node.toString());
			// System.out.println();
		}

		public void initChildren() {
			// TODO lier (binding) this à un modelContext
			System.out.println("ElementViewContext::initChildren(" + node + ")");
			if (template.content != null)
				template.content.forEach(element -> {
					Node childNode = null;
					try {
						childNode = createNode(((AbstractElement) element).classNode);
						registre(childNode);
					} catch (Exception e) {
						throw new IllegalStateException(e);
					}

					if (childNode instanceof Button)
						((Button) childNode).setText(((AbstractElement) element).text);

					ElementViewContext viewContextChild = new ElementViewContext(modelContext, (AbstractElement) element, childNode);
					addChildren(viewContextChild);
					viewContextChild.init();
				});
			System.out.println();
		}

		private Node createNode(Class<? extends Node> clazz) throws InstantiationException, IllegalAccessException {
			System.out.println("ElementViewContext::createNode(" + clazz + ")");
			return clazz.newInstance();
		}

		public void init() {
			System.out.println("ElementViewContext::init viewContext de::(" + node + ")");
			BindingContext bindingContext = new BindingContext(modelContext, this);
			if(template.binding!=null)
				template.binding.init(bindingContext);
			initChildren();
		}

		public void registre(Node node) {
			// System.out.println("ElementViewContext::registre(" + node + ")");
			modelContext.registre(node);
		}

		public void addChildren(AbstractViewContext viewContext) {
			System.out.println("ElementViewContext::addChildren(" + viewContext.node + " à " + node + ")");
			if (node instanceof Pane)
				((Pane) node).getChildren().add(viewContext.node);
			children.add(viewContext);
		}

		@Override
		public void bind(IModelContext model) {
			System.out.println("ElementViewContext::BIND");
			ElementViewContext wrapper = new ElementViewContext(model, template, node);
			wrapper.initChildren();
		}
	}
}
