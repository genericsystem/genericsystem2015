package org.genericsystem.ui;

import java.util.List;

public class ViewContext<N> {
	private final ViewContext<?> parent;
	private final Element<N> template;
	private final N node;

	public ViewContext(ViewContext<?> parent, ModelContext modelContext, Element<N> template, N node) {
		this.parent = parent;
		this.template = template;
		this.node = node != null ? node : template.createNode(parent != null ? parent.getNode() : null);
		init(modelContext);
	}

	private void init(ModelContext modelContext) {
		modelContext.register(this);
		this.template.getBootList().forEach(boot -> boot.init(node));
		for (Binding<N, ?, ?> binding : template.bindings)
			binding.init(modelContext, getNode());
		for (Element<N> childElement : template.<N> getChildren()) {
			for (MetaBinding<N, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(modelContext, this, childElement);
			if (childElement.metaBindings.isEmpty())
				new ViewContext<>(this, modelContext, childElement, null);
		}
		if (parent != null) {
			List<N> graphicChildren = template.uiChildren(parent.getNode());
			int indexInChildren = template.getParent().computeIndex(graphicChildren, template);
			template.getParent().incrementSize(graphicChildren, template);
			graphicChildren.add(indexInChildren, node);
		}
	}

	public N getNode() {
		return node;
	}

	void destroyChild() {
		List<N> uiChildren = template.uiChildren(parent.getNode());
		template.getParent().decrementSize(uiChildren, template);
		uiChildren.remove(getNode());
	}

}
