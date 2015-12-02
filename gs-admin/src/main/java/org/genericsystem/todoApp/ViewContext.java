package org.genericsystem.todoApp;

import javafx.scene.Node;

import org.genericsystem.todoApp.binding.Binding;

public class ViewContext {
	private final Element template;
	private final Node node;
	private final ModelContext modelContext;
	private final ViewContext parent;
	private boolean initChildren = true;

	public ViewContext(ModelContext modelContext, Element template, Node node, ViewContext parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
	}

	public ViewContext init() {
		for (Binding<?> binding : template.bindings)
			binding.init(modelContext, this);
		if (initChildren)
			initChildren();
		return this;
	}

	public ModelContext createChild(Object childModel, ModelContext parentContext) {
		ModelContext childContext = new ModelContext(parentContext, childModel);
		new ViewContext(childContext, template, node, this).initChildren();
		return childContext;
	}

	private void initChildren() {
		for (Element childElement : template.getChildren()) {
			Node childNode = createNode(childElement.classNode);
			childElement.getGraphicChildren(node).add(childNode);
			ViewContext childViewContext = new ViewContext(modelContext, childElement, childNode, this);
			modelContext.register(childViewContext);
			childViewContext.init();
		}
	}

	private void initChildren2() {
		Node parentNode = createNode(getParent().template.classNode);
		for (Element childElement : getParent().template.getChildren()) {
			Node childNode = createNode(childElement.classNode);
			childElement.getGraphicChildren(parentNode).add(childNode);
			ViewContext childViewContext = new ViewContext(modelContext, childElement, childNode, this);
			modelContext.register(childViewContext);
			childViewContext.init();
		}
		getParent().getParent().template.getGraphicChildren(getParent().getParent().getNode()).add(parentNode);
	}

	public void destroyChild() {
		template.getGraphicChildren(getParent().getNode()).remove(getNode());
	}

	private static Node createNode(Class<? extends Node> clazz) {
		try {
			return clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public void disableInitChildren() {
		this.initChildren = false;
	}

	public Element getTemplate() {
		return template;
	}

	public Node getNode() {
		return node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getParent() {
		return parent;
	}
}
