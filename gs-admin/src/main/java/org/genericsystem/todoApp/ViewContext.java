package org.genericsystem.todoApp;

import org.genericsystem.todoApp.binding.Binding;

public class ViewContext {
	private final Element template;
	private final Object node;
	private final ModelContext modelContext;
	private final ViewContext parent;
	private boolean initChildren = true;

	public ViewContext(ModelContext modelContext, Element template, Object node, ViewContext parent) {
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
		Object childNode = template.classNode.isAssignableFrom(childModel.getClass()) ? childModel : template.createNode();
		new ViewContext(childContext, template, childNode, getParent()).initChildren();
		return childContext;
	}

	private void initChildren() {
		for (Element childElement : template.getChildren()) {
			Object childNode = childElement.createNode();
			ViewContext childViewContext = new ViewContext(modelContext, childElement, childNode, this);
			modelContext.register(childViewContext);
			childViewContext.init();
		}
		if (getParent() != null) {
			System.out.println("add node : " + node + " to parent : " + getParent().getNode() + " list = " + template.getGraphicChildren(getParent().getNode()));
			template.getGraphicChildren(getParent().getNode()).add(node);
		}

	}

	public void destroyChild() {
		template.getGraphicChildren(getParent().getNode()).remove(getNode());
	}

	private static Object createNode(Class<?> clazz) {
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

	public Object getNode() {
		return node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getParent() {
		return parent;
	}
}
