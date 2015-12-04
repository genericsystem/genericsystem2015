package org.genericsystem.ui;

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
		this.template.getBootList().forEach(boot -> boot.init(node));
		for (Binding<?, ?, ?> binding : template.bindings)
			binding.init(modelContext, this, null);
		for (Element<CHILDNODE> childElement : template.<CHILDNODE> getChildren()) {
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

	<PARENTNODE> void destroyChild() {
		template.getGraphicChildren(getParent().getNode()).remove(getNode());
	}

	public Element<NODE> getTemplate() {
		return template;
	}

	public NODE getNode() {
		return node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ViewContext<PARENTNODE> getParent() {
		return (ViewContext<PARENTNODE>) parent;
	}
}
