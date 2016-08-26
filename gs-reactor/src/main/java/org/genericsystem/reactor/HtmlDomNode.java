package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.BiConsumer;

import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag.RootTag;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.beans.value.ChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.SetChangeListener;

public class HtmlDomNode<M extends Model> {

	static int count = 0;
	protected static final String MSG_TYPE = "msgType";
	protected static final String ADD = "A";
	protected static final String UPDATE = "U";
	static final String REMOVE = "R";
	static final String UPDATE_TEXT = "UT";
	private static final String UPDATE_SELECTION = "US";
	static final String ADD_STYLECLASS = "AC";
	static final String REMOVE_STYLECLASS = "RC";
	static final String ADD_STYLE = "AS";
	static final String REMOVE_STYLE = "RS";
	static final String ADD_ATTRIBUTE = "AA";
	static final String REMOVE_ATTRIBUTE = "RA";

	static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	static final String NEXT_ID = "nextId";
	static final String STYLE_PROPERTY = "styleProperty";
	static final String STYLE_VALUE = "styleValue";
	static final String ATTRIBUTE_NAME = "attributeName";
	static final String ATTRIBUTE_VALUE = "attributeValue";
	static final String STYLECLASS = "styleClass";
	protected static final String TEXT_CONTENT = "textContent";
	static final String TAG_HTML = "tagHtml";
	protected static final String ELT_TYPE = "eltType";
	protected static final String SELECTED_INDEX = "selectedIndex";

	private final String id;
	private String parentId;// TODO: Remove
	private HtmlDomNode<M> parent;
	private Tag<M> tag;
	private Model modelContext;

	public HtmlDomNode(String parentId, HtmlDomNode<M> parent, Model modelContext, Tag<M> tag) {
		assert parentId != null;
		this.parentId = parentId;
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.parent = parent;
		this.tag = tag;
		this.modelContext = modelContext;
	}

	protected <BETWEEN> void init(int index) {
		modelContext.register(this);
		if (parent != null)
			insertChild(index);
		for (BiConsumer<Model, HtmlDomNode> binding : tag.getPreFixedBindings())
			binding.accept(modelContext, this);
		for (Tag<?> childTag : tag.getObservableChildren()) {
			MetaBinding<BETWEEN> metaBinding = childTag.<BETWEEN> getMetaBinding();
			if (metaBinding != null) {
				modelContext.setSubContexts(childTag, new TransformationObservableList<BETWEEN, Model>(metaBinding.buildBetweenChildren(modelContext), (i, between) -> {
					Model childModel = metaBinding.buildModel(modelContext, between);
					createViewContextChild(i, childModel, childTag);
					return childModel;
				}, Model::destroy));
			} else
				createViewContextChild(null, modelContext, childTag);
		}
		for (BiConsumer<Model, HtmlDomNode> binding : tag.getPostFixedBindings())
			binding.accept(modelContext, this);
	}

	public void createViewContextChild(Integer index, Model childModelContext, Tag element) {
		int indexInChildren = computeIndex(index, element);
		HtmlDomNode<M> node = element.createNode(getId(), this, childModelContext, element);
		node.init(indexInChildren);
	}

	private int computeIndex(Integer nullable, Tag<?> childElement) {
		int indexInChildren = nullable == null ? sizeBySubElement.get(childElement) : nullable;
		for (Tag<?> child : tag.getObservableChildren()) {
			if (child == childElement)
				return indexInChildren;
			indexInChildren += sizeBySubElement.get(child);
		}
		return indexInChildren;
	}

	@SuppressWarnings("unchecked")
	public <MODEL extends Model> MODEL getModelContext() {
		return (MODEL) modelContext;
	}

	protected RootHtmlDomNode<M> getRootHtmlDomNode() {
		return parent.getRootHtmlDomNode();
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
		parent.incrementSize(tag);
		sendAdd(index);
		getRootHtmlDomNode().add(getId(), this);
	}

	private boolean destroyed = false;

	void destroy() {
		// System.out.println("Attempt to destroy : " + getNode().getId());
		assert !destroyed : "Node : " + getId();
		destroyed = true;
		getRootHtmlDomNode().remove(getId());
		parent.decrementSize(tag);
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

	public ServerWebSocket getWebSocket() {
		return parent.getWebSocket();
	}

	private final MapChangeListener<String, String> stylesListener = change -> {
		if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()).put(STYLE_VALUE, change.getValueAdded()));
	};

	private final MapChangeListener<String, String> attributesListener = change -> {
		if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()).put(ATTRIBUTE_VALUE, change.getValueAdded()));
	};

	private final SetChangeListener<String> styleClassesListener = change -> {
		if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementAdded()));
		else
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementRemoved()));
	};

	private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));

	private final ChangeListener<Number> indexListener = (o, old, newValue) -> {
		// System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0)
		// .encodePrettily());
		sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0));
	};

	public ChangeListener<Number> getIndexListener() {
		return indexListener;
	}

	public ChangeListener<String> getTextListener() {
		return textListener;
	}

	public MapChangeListener<String, String> getStylesListener() {
		return stylesListener;
	}

	public MapChangeListener<String, String> getAttributesListener() {
		return attributesListener;
	}

	public SetChangeListener<String> getStyleClassesListener() {
		return styleClassesListener;
	}

	public void sendAdd(int index) {
		JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
		jsonObj.put(PARENT_ID, parentId);
		jsonObj.put(ID, id);
		jsonObj.put(TAG_HTML, getTag().getTag());
		jsonObj.put(NEXT_ID, index);
		fillJson(jsonObj);
		// System.out.println(jsonObj.encodePrettily());
		sendMessage(jsonObj);
	}

	public JsonObject fillJson(JsonObject jsonObj) {
		return null;
	}

	public void sendRemove() {
		sendMessage(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id));
		// System.out.println(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id).encodePrettily());
	}

	public void sendMessage(JsonObject jsonObj) {
		jsonObj.put("count", count++);
		// if (jsonObj.getString(MSG_TYPE).equals(ADD) || jsonObj.getString(MSG_TYPE).equals(REMOVE))
		// System.out.println(jsonObj.encodePrettily());
		getWebSocket().writeFinalTextFrame(jsonObj.encode());
	}

	public String getId() {
		return id;
	}

	public Tag<M> getTag() {
		return tag;
	}

	public void handleMessage(JsonObject json) {

	}

	public static class RootHtmlDomNode<M extends Model> extends HtmlDomNode<M> {
		private final Map<String, HtmlDomNode> nodeById = new HashMap<>();
		private final ServerWebSocket webSocket;

		public RootHtmlDomNode(M rootModelContext, RootTag<M> template, String rootId, ServerWebSocket webSocket) {
			super(rootId, null, rootModelContext, (Tag<M>) template);
			this.webSocket = webSocket;
			sendAdd(0);
			init(0);
		}

		@Override
		public ServerWebSocket getWebSocket() {
			return webSocket;
		}

		@Override
		protected RootHtmlDomNode<M> getRootHtmlDomNode() {
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
}