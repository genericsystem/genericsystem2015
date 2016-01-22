package org.genericsystem.ui;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Function;

import javafx.collections.ObservableList;

public class ViewContext<N> {
	private final ViewContext<?> parent;
	private final Element<N> template;
	private final N node;

	private final ObservableList<N> nodeChildren;

	public ViewContext(ViewContext<?> parent, ModelContext modelContext, Element<N> template, N node) {
		this.parent = parent;
		this.template = template;
		this.node = node != null ? node : template.createNode(parent != null ? parent.getNode() : null);
		nodeChildren = this.parent != null ? (ObservableList<N>) ((Function) this.template.getGraphicChildren).apply(parent.node) : null;
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
			int indexInChildren = parent.computeIndex(template);
			parent.incrementSize(template);
			nodeChildren.add(indexInChildren, node);
			map2.put(template, indexInChildren);

		}
	}

	public N getNode() {
		return node;
	}

	Map<Element<?>, Integer> map2 = new IdentityHashMap<Element<?>, Integer>() {
		private static final long serialVersionUID = 1L;

		@Override
		public Integer get(Object key) {
			Integer size = super.get(key);
			if (size == null)
				put((Element<?>) key, size = 0);
			return size;
		};
	};

	void destroyChild() {
		parent.decrementSize(template);
		nodeChildren.remove(getNode());
	}

	void incrementSize(Element<?> child) {
		map2.put(child, map2.get(child) + 1);
	}

	void decrementSize(Element<?> child) {
		int size = map2.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			map2.remove(child);// remove map if 0 for avoid heap pollution
		else
			map2.put(child, size);
	}

	int computeIndex(Element<?> childElement) {
		int indexInChildren = 0;
		for (Element<?> child : template.getChildren()) {
			indexInChildren += map2.get(child);
			if (child == childElement)
				break;
		}
		return indexInChildren;
	}

}
