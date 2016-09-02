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

public class HtmlDomNode {

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
	private HtmlDomNode parent;
	private Tag tag;
	private Context modelContext;

	public HtmlDomNode(HtmlDomNode parent, Context modelContext, Tag tag) {
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.parent = parent;
		this.tag = tag;
		this.modelContext = modelContext;
	}

	// <BETWEEN> ListChangeListener<Tag> getListChangeListener() {
	// return change -> {
	// while (change.next()) {
	// if (change.wasRemoved()) {
	// for (Tag childTag : change.getRemoved()) {
	// MetaBinding<BETWEEN> metaBinding = childTag.<BETWEEN> getMetaBinding();
	// if (metaBinding != null) {
	// modelContext.getSubContexts(childTag).removeAll();// destroy subcontexts
	// modelContext.removeSubContexts(childTag);// remove tag ref
	// }
	// sizeBySubTag.remove(childTag);
	// }
	// }
	// if (change.wasAdded()) {
	// int index = change.getFrom();
	// for (Tag childTag : change.getAddedSubList()) {
	// MetaBinding<BETWEEN> metaBinding = childTag.<BETWEEN> getMetaBinding();
	// if (metaBinding != null) {
	// modelContext.setSubContexts(childTag, new
	// TransformationObservableList<BETWEEN,
	// Context>(metaBinding.buildBetweenChildren(modelContext), (i, between) ->
	// {
	// Context childModel = metaBinding.buildModel(modelContext, between);
	// createChildDomNode(i, childModel, childTag);
	// return childModel;
	// }, Context::destroy));
	// } else
	// createChildDomNode(index++, modelContext, childTag);
	// }
	// }
	// }
	//
	// };
	// }

	protected <BETWEEN> void init(int index) {
		modelContext.register(this);
		if (parent != null)
			insertChild(index);
		for (BiConsumer<Context, HtmlDomNode> binding : tag.getPreFixedBindings())
			binding.accept(modelContext, this);
		for (Tag childTag : tag.getObservableChildren()) {
			MetaBinding<BETWEEN> metaBinding = childTag.<BETWEEN> getMetaBinding();
			if (metaBinding != null) {
				modelContext.setSubContexts(childTag, new TransformationObservableList<BETWEEN, Context>(metaBinding.buildBetweenChildren(modelContext), (i, between) -> {
					Context childModel = metaBinding.buildModel(modelContext, between);
					createChildDomNode(i, childModel, childTag);
					return childModel;
				}, Context::destroy));
			} else
				createChildDomNode(0, modelContext, childTag);
		}
		for (BiConsumer<Context, HtmlDomNode> binding : tag.getPostFixedBindings())
			binding.accept(modelContext, this);
	}

	public void createChildDomNode(int index, Context childContext, Tag childTag) {

		childTag.createNode(this, childContext).init(computeIndex(index, childTag));
		if (childContext.isOpaque())
			childTag.addStyleClass(childContext, "opaque");
	}

	private int computeIndex(int indexInChildren, Tag childElement) {
		for (Tag child : tag.getObservableChildren()) {
			if (child == childElement)
				return indexInChildren;
			indexInChildren += sizeBySubTag.get(child);
		}
		return indexInChildren;
	}

	public Context getModelContext() {
		return modelContext;
	}

	protected RootHtmlDomNode getRootHtmlDomNode() {
		return parent.getRootHtmlDomNode();
	}

	private Map<Tag, Integer> sizeBySubTag = new IdentityHashMap<Tag, Integer>() {
		private static final long serialVersionUID = 6725720602283055930L;

		@Override
		public Integer get(Object key) {
			Integer size = super.get(key);
			if (size == null)
				put((Tag) key, size = 0);
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

	private void incrementSize(Tag child) {
		sizeBySubTag.put(child, sizeBySubTag.get(child) + 1);
	}

	private void decrementSize(Tag child) {
		int size = sizeBySubTag.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			sizeBySubTag.remove(child);// remove map if empty
		else
			sizeBySubTag.put(child, size);
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
		// System.out.println(new JsonObject().put(MSG_TYPE,
		// UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue !=
		// null ? newValue : 0)
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
		jsonObj.put(PARENT_ID, getParentId());
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
		// System.out.println(new JsonObject().put(MSG_TYPE, REMOVE).put(ID,
		// id).encodePrettily());
	}

	public void sendMessage(JsonObject jsonObj) {
		jsonObj.put("count", count++);
		// if (jsonObj.getString(MSG_TYPE).equals(ADD) ||
		// jsonObj.getString(MSG_TYPE).equals(REMOVE))
		// System.out.println(jsonObj.encodePrettily());
		getWebSocket().writeFinalTextFrame(jsonObj.encode());
	}

	public String getId() {
		return id;
	}

	public String getParentId() {
		return parent.getId();
	}

	public Tag getTag() {
		return tag;
	}

	public void handleMessage(JsonObject json) {

	}

	public static class RootHtmlDomNode extends HtmlDomNode {
		private final Map<String, HtmlDomNode> nodeById = new HashMap<>();
		private final ServerWebSocket webSocket;
		private final String rootId;

		public RootHtmlDomNode(Context rootModelContext, RootTag rootTag, String rootId, ServerWebSocket webSocket) {
			super(null, rootModelContext, (Tag) rootTag);
			this.rootId = rootId;
			this.webSocket = webSocket;
			sendAdd(0);
			init(0);
		}

		@Override
		public ServerWebSocket getWebSocket() {
			return webSocket;
		}

		@Override
		protected RootHtmlDomNode getRootHtmlDomNode() {
			return this;
		}

		@Override
		public String getParentId() {
			return rootId;
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