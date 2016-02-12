package org.genericsystem.distributed.cacheonserver.ui.js;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.distributed.cacheonserver.ui.js.ModelContext.RootModelContext;

public class ViewContext<N> {

	private final ViewContext<?> parent;
	private final Element<N> template;
	private final N node;
	private ModelContext modelContext;

	private final ObservableList<N> nodeChildren;

	private ViewContext(ViewContext<?> parent, ModelContext modelContext, Element<N> template, N node) {
//		System.out.println("viewContext constructor");
		this.parent = parent;
		this.template = template;
		assert node != null;
		this.node = node;
		this.modelContext = modelContext;
		nodeChildren = this.parent != null ? (ObservableList<N>) ((Function) this.template.getGraphicChildren).apply(parent.node) : null;

		init();
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public <SUBNODE> ViewContext<SUBNODE> createChildContext(ModelContext childModelContext, Element<SUBNODE> template) {
		return new ViewContext<SUBNODE>(this, childModelContext, template, template.createNode(getNode()));
	}

	private void init() {
//		System.out.println("init");
		modelContext.register(this);
		
		this.template.getBootList().forEach(boot -> boot.init(node));
		
		for (Binding<N, ?, ?> binding : template.bindings)
			binding.init(modelContext, getNode());
		if (parent != null) {
			int indexInChildren = parent.computeIndex(template);
			parent.incrementSize(template);
			nodeChildren.add(indexInChildren, node);
			sizeByElement.put(template, indexInChildren);
		}
		for (Element<N> childElement : template.<N> getChildren()) {
			for (MetaBinding<N, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(this, childElement);
			if (childElement.metaBindings.isEmpty())
				createChildContext(modelContext, childElement);
		}
	}

	private RootViewContext getRootViewContext() {
		ViewContext<?> parent = this.parent;
		while (parent != null) {
			try {
				return (RootViewContext) parent;
			} catch (ClassCastException ignore) {
			}
			parent = parent.parent;
		}
		throw new IllegalStateException("");
	}

	public N getNode() {
		return node;
	}

	private Map<Element<?>, Integer> sizeByElement = new IdentityHashMap<Element<?>, Integer>() {
		private static final long serialVersionUID = 6725720602283055930L;

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

	private void incrementSize(Element<?> child) {
		sizeByElement.put(child, sizeByElement.get(child) + 1);
	}

	private void decrementSize(Element<?> child) {
		int size = sizeByElement.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			sizeByElement.remove(child);// remove map if empty
		else
			sizeByElement.put(child, size);
	}

	private int computeIndex(Element<?> childElement) {
		int indexInChildren = 0;
		for (Element<?> child : template.getChildren()) {
			indexInChildren += sizeByElement.get(child);
			if (child == childElement)
				break;
		}
		return indexInChildren;
	}

	public static class RootViewContext extends ViewContext<NodeJs> {
		public RootViewContext(Model model, Element<NodeJs> template, NodeJs node) {
			super(null, new RootModelContext(model), template, node);
		}
	}
}
