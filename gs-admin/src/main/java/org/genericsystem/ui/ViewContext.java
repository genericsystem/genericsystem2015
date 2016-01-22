package org.genericsystem.ui;

import java.util.IdentityHashMap;
import java.util.List;
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
			int indexInChildren = parent.computeIndex(nodeChildren, template);
			parent.incrementSize(nodeChildren, template);
			nodeChildren.add(indexInChildren, node);

		}
	}

	public N getNode() {
		return node;
	}

	void destroyChild() {
		parent.decrementSize(nodeChildren, template);
		nodeChildren.remove(getNode());
	}

	// Map<Element, Integer> map2 = new IdentityHashMap<Element, Integer>() {
	// @Override
	// public Integer get(Object key) {
	// Integer size = super.get(key);
	// if (size == null)
	// put((Element) key, size = 0);
	// return size;
	// };
	// };

	private Map<List, Map<Element, Integer>> map = new IdentityHashMap<List, Map<Element, Integer>>() {
		@Override
		public Map<Element, Integer> get(Object key) {
			Map<Element, Integer> internal = super.get(key);
			if (internal == null)
				put((List) key, internal = new IdentityHashMap<Element, Integer>() {
					@Override
					public Integer get(Object key) {
						Integer size = super.get(key);
						if (size == null)
							put((Element) key, size = 0);
						return size;
					};
				});
			return internal;
		};
	};

	void incrementSize(List uiChildren, Element child) {
		Map<Element, Integer> internal = map.get(uiChildren);
		internal.put(child, internal.get(child) + 1);
	}

	void decrementSize(List uiChildren, Element child) {
		Map<Element, Integer> internal = map.get(uiChildren);
		int size = internal.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			internal.remove(child);// remove map if 0 for avoid heap pollution
		else
			internal.put(child, size);
	}

	int computeIndex(List uiChildren, Element childElement) {
		int indexInChildren = 0;
		for (Element child : template.getChildren()) {
			indexInChildren += map.get(uiChildren).get(child);
			if (child == childElement)
				break;
		}
		return indexInChildren;
	}

}
