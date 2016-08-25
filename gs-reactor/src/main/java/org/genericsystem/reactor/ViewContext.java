package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.BiConsumer;

import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag.RootTag;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public class ViewContext<M extends Model> {

	private ViewContext<M> parent;
	private Tag<M> element;
	protected HtmlDomNode node;
	private Model modelContext;

	private ViewContext() {
	}

	private ViewContext(int indexInChildren, ViewContext<M> parent, Model modelContext, Tag<M> element) {
		init(parent, modelContext, element, element.createNode(parent.getNode().getId()));
		init(indexInChildren);
	}

	protected void init(ViewContext<M> parent, Model modelContext, Tag<M> element, HtmlDomNode node) {
		this.parent = parent;
		this.element = element;
		assert node != null;
		this.node = node;
		node.viewContext = this;
		this.modelContext = modelContext;
	}

	protected <SUBELEMENT> void init(int indexInChildren) {
		modelContext.register(this);
		if (parent != null)
			insertChild(indexInChildren);
		for (BiConsumer<Model, HtmlDomNode> binding : element.getPreFixedBindings())
			binding.accept(modelContext, getNode());
		for (Tag childTag : element.getChildren())
			if (childTag.getMetaBinding() != null) {
				Model model = getModelContext();
				ObservableList<SUBELEMENT> subElements = (ObservableList<SUBELEMENT>) childTag.getMetaBinding().apply(model);
				ObservableList<M> subModels = new TransformationObservableList<SUBELEMENT, M>(subElements, (index, subModel) -> {
					M resultModel = (M) childTag.getModelBuilder().apply(model, subModel);
					createViewContextChild(index, resultModel, childTag);
					return resultModel;
				}, Model::destroy);
				element.setSubModels(model, childTag, subModels);
			} else
				createViewContextChild(null, modelContext, childTag);
		for (BiConsumer<Model, HtmlDomNode> binding : element.getPostFixedBindings())
			binding.accept(modelContext, getNode());
	}

	@SuppressWarnings("unchecked")
	public <MODEL extends Model> MODEL getModelContext() {
		return (MODEL) modelContext;
	}

	public ViewContext<M> createViewContextChild(Integer index, Model childModelContext, Tag<M> element) {
		int indexInChildren = computeIndex(index, element);
		return new ViewContext<M>(indexInChildren, this, childModelContext, element);
	}

	protected RootViewContext<M> getRootViewContext() {
		return parent.getRootViewContext();
	}

	@SuppressWarnings({ "unchecked" })
	public <NODE extends HtmlDomNode> NODE getNode() {
		return (NODE) node;
	}

	private Map<Tag<?>, Integer> sizeBySubElement = new IdentityHashMap<Tag<?>, Integer>() {
		private static final long serialVersionUID = 6725720602283055930L;

		@Override
		public Integer get(Object key) {
			Integer size = super.get(key);
			if (size == null)
				put((Tag<?>) key, size = 0);
			return size;
		};
	};

	void insertChild(int index) {
		parent.incrementSize(element);
		node.sendAdd(index);
		getRootViewContext().add(node.getId(), node);
	}

	private boolean destroyed = false;

	void destroy() {
		// System.out.println("Attempt to destroy : " + getNode().getId());
		assert !destroyed : "Node : " + getNode().getId();
		destroyed = true;
		getRootViewContext().remove(node.getId());
		parent.decrementSize(element);
	}

	private void incrementSize(Tag<?> child) {
		sizeBySubElement.put(child, sizeBySubElement.get(child) + 1);
	}

	private void decrementSize(Tag<?> child) {
		int size = sizeBySubElement.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			sizeBySubElement.remove(child);// remove map if empty
		else
			sizeBySubElement.put(child, size);
	}

	private int computeIndex(Integer nullable, Tag<?> childElement) {
		int indexInChildren = nullable == null ? sizeBySubElement.get(childElement) : nullable;
		for (Tag<?> child : element.getChildren()) {
			if (child == childElement)
				return indexInChildren;
			indexInChildren += sizeBySubElement.get(child);
		}
		return indexInChildren;
	}

	public ServerWebSocket getWebSocket() {
		return parent.getWebSocket();
	}

	public static class RootViewContext<M extends Model> extends ViewContext<M> {
		private final Map<String, HtmlDomNode> nodeById = new HashMap<>();
		private final ServerWebSocket webSocket;

		public RootViewContext(M rootModelContext, RootTag<M> template, String rootId, ServerWebSocket webSocket) {
			this.webSocket = webSocket;
			init(null, rootModelContext, (Tag<M>) template, new HtmlDomNode(rootId));
			node.sendAdd(0);
			init(0);
		}

		@Override
		public ServerWebSocket getWebSocket() {
			return webSocket;
		}

		@Override
		protected RootViewContext<M> getRootViewContext() {
			return this;
		}

		private Map<String, HtmlDomNode> getMap() {
			return nodeById;
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

	public Tag<M> getTag() {
		return element;
	}
}