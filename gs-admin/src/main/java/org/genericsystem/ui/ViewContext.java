package org.genericsystem.ui;

import javafx.collections.ObservableList;

public class ViewContext<NODE> {
	private final Element<NODE> template;
	private final NODE node;
	private final ModelContext modelContext;
	private final ViewContext<?> parent;

	public <PARENTNODE, CHILDNODE> ViewContext(ModelContext modelContext, Element<NODE> template, NODE node, ViewContext<?> parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
		modelContext.register(this);
		for (Binding<?, ?, ?> binding : template.bindings)
			binding.init(modelContext, this, null);
		for (Element<CHILDNODE> childElement : template.getChildren()) {
			for (Binding<?, ?, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(modelContext, this, childElement);
			if (childElement.metaBindings.isEmpty()) {
				new ViewContext<CHILDNODE>(modelContext, childElement, childElement.createNode(), this);
			}
		}
		if (getParent() != null) {
			// System.out.println("add node : " + node + " to parent : " + getParent().getNode() + " list = " + template.getGraphicChildren(getParent().getNode()));
			((ObservableList<NODE>) (template.getGraphicChildren((PARENTNODE) getParent().getNode()))).add(node);
		}
	}

	<PARENTNODE> void destroyChild() {
		template.getGraphicChildren((PARENTNODE) getParent().getNode()).remove(getNode());
	}

	public Element<NODE> getTemplate() {
		return template;
	}

	@SuppressWarnings("unchecked")
	public NODE getNode() {
		return node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext<?> getParent() {
		return parent;
	}
}
