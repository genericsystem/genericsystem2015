package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

import org.genericsystem.reactor.Element.HtmlDomNode;
import org.genericsystem.reactor.ModelContext.RootModelContext;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public class ViewContext<M extends Model> {

	private final ViewContext<?> parent;
	private final Element<M> template;
	private final HtmlDomNode node;
	private ModelContext modelContext;

	private ViewContext(int indexInChildren, ViewContext<?> parent, ModelContext modelContext, Element<M> template, HtmlDomNode node) {
		this.parent = parent;
		this.template = template;
		assert node != null;
		this.node = node;
		this.modelContext = modelContext;
		modelContext.register(this);

		if (parent != null) {
			insertChild(indexInChildren);
		}
		this.template.getBootList().forEach(boot -> boot.init(node));

		for (Binding<?, ?> binding : template.bindings)
			binding.init(modelContext, getNode());

		for (Element<?> childElement : template.getChildren()) {
			if (childElement.metaBinding != null)
				childElement.metaBinding.init(this, childElement);
			else
				createChildContext(null, modelContext, childElement);
		}
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext<?> createChildContext(Integer index, ModelContext childModelContext, Element<?> template) {
		int indexInChildren = computeIndex(index, template);
		return new ViewContext<>(indexInChildren, this, childModelContext, template, template.createNode(node.getId()));
	}

	protected RootViewContext<?> getRootViewContext() {
		return parent.getRootViewContext();
	}

	public HtmlDomNode getNode() {
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

	void insertChild(int index) {
		parent.incrementSize(template);
		node.sendAdd(index);
		getRootViewContext().add(node.getId(), node);
	}

	void destroyChild() {
		parent.decrementSize(template);
		node.sendRemove();
		getRootViewContext().remove(node.getId());

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

	public static class RootViewContext<M extends Model> extends ViewContext<M> {
		private Map<String, HtmlDomNode> nodeById;

		public RootViewContext(Model model, Element<M> template, HtmlDomNode node) {
			super(0, null, new RootModelContext(model), template, node);
		}

		@Override
		protected RootViewContext<?> getRootViewContext() {
			return this;
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
