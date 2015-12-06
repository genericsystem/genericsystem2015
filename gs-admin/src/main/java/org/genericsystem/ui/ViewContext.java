package org.genericsystem.ui;

public class ViewContext<N> {
	private final Element<N> template;
	private final N node;
	// private final ModelContext modelContext;
	private final ViewContext<?> parent;

	public <PARENTNODE, CHILDNODE> ViewContext(ModelContext modelContext, Element<N> template, N node, ViewContext<?> parent) {
		this.template = template;
		this.node = node;
		this.parent = parent;
		// this.modelContext = modelContext;
		modelContext.register(this);
		this.template.getBootList().forEach(boot -> boot.init(node));
		for (Binding<?, ?, ?> binding : template.bindings)
			binding.init(modelContext, this, null);
		for (Element<CHILDNODE> childElement : template.<CHILDNODE> getChildren()) {
			for (Binding<?, ?, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(modelContext, this, (Element) childElement);
			if (childElement.metaBindings.isEmpty())
				new ViewContext<>(modelContext, childElement, childElement.createNode(), this);
		}
		if (parent != null) {
			// System.out.println("add node : " + node + " to parent : " + getParent().getNode() + " list = " + template.getGraphicChildren(getParent().getNode()));
			template.getGraphicChildren(parent.getNode()).add(node);
		}
	}

	public N getNode() {
		return node;
	}

	<PARENTNODE> void destroyChild() {
		template.getGraphicChildren(parent.getNode()).remove(getNode());
	}

	// public Element<NODE> getTemplate() {
	// return template;
	// }

	// public ModelContext getModelContext() {
	// return modelContext;
	// }

	// @SuppressWarnings("unchecked")
	// public <PARENTNODE> ViewContext<PARENTNODE> getParent() {
	// return (ViewContext<PARENTNODE>) parent;
	// }
}
