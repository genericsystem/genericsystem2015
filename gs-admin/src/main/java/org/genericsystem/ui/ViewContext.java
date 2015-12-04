package org.genericsystem.ui;

public class ViewContext<NODE> {
	private final Element<NODE> template;
	private final Object node;
	private final ModelContext modelContext;
	private final ViewContext<?> parent;

	public ViewContext(ModelContext modelContext, Element<NODE> template, Object node, ViewContext<?> parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
		modelContext.register(this);
		for (Binding<?, ?, ?> binding : template.bindings)
			binding.init(modelContext, this, null);
		for (Element<?> childElement : template.getChildren()) {
			for (Binding<?, ?, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(modelContext, this, childElement);
			if (childElement.metaBindings.isEmpty()) {
				new ViewContext<>(modelContext, childElement, childElement.createNode(), this);
			}
		}
		if (getParent() != null) {
			// System.out.println("add node : " + node + " to parent : " + getParent().getNode() + " list = " + template.getGraphicChildren(getParent().getNode()));
			template.getGraphicChildren(getParent().getNode()).add(node);
		}
	}

	void destroyChild() {
		template.getGraphicChildren(getParent().getNode()).remove(getNode());
	}

	public Element<NODE> getTemplate() {
		return template;
	}

	@SuppressWarnings("unchecked")
	public NODE getNode() {
		return (NODE) node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext<?> getParent() {
		return parent;
	}
}
