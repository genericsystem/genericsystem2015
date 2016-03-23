package org.genericsystem.distributed.ui;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.genericsystem.distributed.ui.ModelContext.RootModelContext;

public class ViewContext<N> {

	private final ViewContext<?> parent;
	private final Element<N> template;
	private final N node;
	private ModelContext modelContext;

	private final List<N> nodeChildren;

	private ViewContext(Integer index, ViewContext<?> parent, ModelContext modelContext, Element<N> template, N node) {
		this.parent = parent;
		this.template = template;
		assert node != null;
		this.node = node;
		this.modelContext = modelContext;
		nodeChildren = this.parent != null ? (List<N>) ((Function) template.getParent().getGraphicChildren()).apply(parent.node) : null;
		init(index);
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public <SUBNODE> ViewContext<SUBNODE> createChildContext(Integer index, ModelContext childModelContext, Element<SUBNODE> template) {
		return new ViewContext<>(index, this, childModelContext, template, template.createNode(getNode()));
	}

	private void init(Integer index) {
		// System.out.println("init");
		modelContext.register(this);

		this.template.getBootList().forEach(boot -> boot.init(node));

		for (Binding<N, ?, ?> binding : template.bindings)
			binding.init(modelContext, getNode());
		if (parent != null) {
			int indexInChildren = parent.computeIndex(index, template);
			parent.incrementSize(template);
			nodeChildren.add(indexInChildren, node);
			if (node instanceof HtmlDomNode)
				getRootViewContext().add(((HtmlDomNode) node).getId(), (HtmlDomNode) node);
			// sizeByElement.put(template, indexInChildren);
		}
		for (Element<N> childElement : template.<N> getChildren()) {
			for (MetaBinding<N, ?> metaBinding : childElement.metaBindings)
				metaBinding.init(this, childElement);
			if (childElement.metaBindings.isEmpty())
				createChildContext(null, modelContext, childElement);
		}
	}

	@SuppressWarnings("unchecked")
	private RootViewContext<N> getRootViewContext() {
		ViewContext<?> parent = this.parent;
		while (parent != null) {

			try {
				return (RootViewContext<N>) parent;
			} catch (ClassCastException ignore) {}
			parent = parent.parent;
		}
		throw new IllegalStateException("parent null");
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
		// TODO remove ids from viewrootcontext

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

	private int computeIndex(Integer nullable, Element<?> childElement) {
		int indexInChildren = nullable == null ? sizeByElement.get(childElement) : nullable;
		for (Element<?> child : template.getChildren()) {
			if (child == childElement)
				return indexInChildren;
			indexInChildren += sizeByElement.get(child);
		}
		return indexInChildren;
	}

	public static class RootViewContext<N> extends ViewContext<N> {
		private Map<String, HtmlDomNode> nodeById;

		public RootViewContext(Model model, Element<N> template, N node) {
			super(null, null, new RootModelContext(model), template, node);
		}

		private Map<String, HtmlDomNode> getMap() {
			return nodeById != null ? nodeById : (nodeById = new HashMap<>());
		}

		public HtmlDomNode getNodeById(String id) {
			return getMap().get(id);
		}

		public void add(String id, HtmlDomNode domNode) {
			getMap().put(id, domNode);
		}

		public void remove(String id) {
			getMap().remove(id);
		}
	}
}
