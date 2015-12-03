package org.genericsystem.ui;

public class ViewContext {
	private final Element template;
	private final Object node;
	private final ModelContext modelContext;
	private final ViewContext parent;

	public ViewContext(ModelContext modelContext, Element template, Object node, ViewContext parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
		modelContext.register(this);
		for (Binding<?> binding : template.bindings)
			binding.init(modelContext, this, null);
		for (Element childElement : template.getChildren()) {
			for (Binding<?> metaBinding : childElement.metaBindings)
				metaBinding.init(modelContext, this, childElement);
			if (childElement.metaBindings.isEmpty()) {
				new ViewContext(modelContext, childElement, childElement.createNode(), this);
			}
		}
		if (getParent() != null) {
			System.out.println("add node : " + node + " to parent : " + getParent().getNode() + " list = " + template.getGraphicChildren(getParent().getNode()));
			template.getGraphicChildren(getParent().getNode()).add(node);
		}
	}

	void destroyChild() {
		template.getGraphicChildren(getParent().getNode()).remove(getNode());
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
