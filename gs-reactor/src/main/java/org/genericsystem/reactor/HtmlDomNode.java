package org.genericsystem.reactor;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.defaults.tools.TransformationObservableList;

import io.vertx.core.json.JsonObject;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.SetChangeListener;
import javafx.collections.transformation.FilteredList;

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
	private Context context;

	public static interface Sender {
		public void send(String message);
	}

	private final Consumer<Tag> tagAdder = tagAdder();
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
	private ListChangeListener<Tag> tagListener = change -> {
		while (change.next()) {
			if (change.wasRemoved()) {
				for (Tag childTag : change.getRemoved()) {
					deepRemove(context, childTag, childTag.getMetaBinding());
					sizeBySubTag.remove(childTag);
				}
			}
			if (change.wasAdded())
				change.getAddedSubList().forEach(tagAdder::accept);
		}
	};

	public List<HtmlDomNode> getChildren() {
		List<HtmlDomNode> result = new ArrayList<>();
		List<Tag> subTags = tag.getObservableChildren();
		for (Tag subTag : subTags) {
			if (subTag.getMetaBinding() == null)
				result.add(context.getHtmlDomNode(subTag));
			else
				for (Context subContext : context.getSubContexts(subTag))
					result.add(subContext.getHtmlDomNode(subTag));
		}
		return result;
	}

	public String header() {
		String header = "";
		String appName = this.tag.getClass().getSimpleName().toLowerCase();
		header = "<!DOCTYPE html>\n";
		header += "<html>\n";
		header += "<head>\n";
		header += "<meta charset=\"UTF-8\" name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n";
		header += "<LINK rel=stylesheet type=\"text/css\" href=\"" + appName + ".css\"/>\n";
		header += "<LINK rel=stylesheet type=\"text/css\" href=\"reactor.css\"/>\n";
		header += "<script>\n";
		header += "var serviceLocation = \"ws://\" + document.location.host + \"" + "\";\n";
		header += "</script>\n";
		header += "<script type=\"text/javascript\" src=\"" + appName + ".js\"></script>\n";
		header += "</head>\n";
		header += "<body onload=\"connect();\" id=\"root\">\n";
		return header;
	}

	public String footer() {
		String footer = "</body>\n";
		footer += "</html>\n";
		return footer;
	}

	public String toHTMLString(String body) {
		String tagText = this.tag.getTag();
		String classes = tag.getDomNodeStyleClasses(context).stream().collect(Collectors.joining(" "));

		classes = ("section".equals(tagText) || "div".equals(tagText) || "header".equals(tagText) || "footer".equals(tagText)) ? classes += " adding" : classes;

		String styles = tag.getDomNodeStyles(context).entrySet().stream().map(m -> m.getKey() + ": " + m.getValue()).collect(Collectors.joining("; "));
		body = "\n<" + tagText + " id=\"" + this.id + "\"";
		if (!classes.equals(""))
			body += " class=\"" + classes + "\"";
		if (!styles.equals(""))
			body += " style=\"" + styles + "\"";

		String tagAttributes = tag.getDomNodeAttributes(context).entrySet().stream().filter(m -> m.getValue() != null && !m.getValue().isEmpty()).map(m -> m.getKey() + "=\"" + m.getValue() + "\"").collect(Collectors.joining(""));
		body += tagAttributes + ">";

		for (HtmlDomNode node : getChildren())
			body += node.toHTMLString(body);
		String tagValue = tag.getDomNodeTextProperty(context).getValue();
		if (tagValue != null)
			body += tagValue;

		body += "</" + tagText + ">";
		return body;
	}

	public void toHtmlFile(String sourceCode, String extention, String path) {
		BufferedWriter writer = null;
		try {
			writer = new BufferedWriter(new FileWriter(path + "index." + extention));
			writer.write(sourceCode);
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public HtmlDomNode(HtmlDomNode parent, Context context, Tag tag) {
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.parent = parent;
		this.tag = tag;
		this.context = context;
		tag.getRootTag().initDomNode(this);
	}

	private <BETWEEN> Consumer<Tag> tagAdder() {
		return childTag -> {
			Property<MetaBinding<BETWEEN>> metaBinding = childTag.getMetaBindingProperty();
			metaBinding.addListener((o, v, nv) -> {
				deepRemove(context, childTag, v);
				sizeBySubTag.remove(childTag);
				updateMetaBinding(childTag, nv);
			});
			updateMetaBinding(childTag, metaBinding.getValue());
		};
	}

	private <BETWEEN> void updateMetaBinding(Tag childTag, MetaBinding<BETWEEN> metaBinding) {
		if (metaBinding != null) {
			if (context.getSubContexts(childTag) == null)
				context.setSubContexts(childTag, new TransformationObservableList<BETWEEN, Context>(metaBinding.buildBetweenChildren(context), (i, between) -> {
					Context childContext = metaBinding.buildModel(context, between);
					childTag.createNode(this, childContext).init(computeIndex(i, childTag));
					if (childContext.isOpaque())
						childTag.addStyleClass(childContext, "opaque");
					return childContext;
				}, Context::destroy));
		} else if (context.getHtmlDomNode(childTag) == null)
			childTag.createNode(this, context).init(computeIndex(0, childTag));
	}

	private boolean destroyed = false;

	void destroy() {
		// System.out.println("Attempt to destroy : " + getNode().getId());
		assert !destroyed : "Node : " + getId();
		destroyed = true;
		sendRemove();
		getRootHtmlDomNode().remove(getId());
		parent.decrementSize(tag);
	}

	private void deepRemove(Context context, Tag tag, MetaBinding<?> oldMetaBinding) {
		if (oldMetaBinding == null) {
			for (Tag childTag : context.getRootContext().getObservableChildren(tag))
				deepRemove(context, childTag, childTag.getMetaBinding());
			if (context.getHtmlDomNode(tag) != null)
				context.getHtmlDomNode(tag).destroy();
			context.removeProperties(tag);
			context.removeHtmlDomNode(tag);
		} else if (context.getSubContexts(tag) != null) {
			for (Context subContext : context.getSubContexts(tag)) {
				for (Tag childTag : context.getRootContext().getObservableChildren(tag))
					deepRemove(subContext, childTag, childTag.getMetaBinding());
				if (subContext.getHtmlDomNode(tag) != null)
					subContext.getHtmlDomNode(tag).destroy();
				subContext.removeProperties(tag);
			}
			context.removeSubContexts(tag);// remove tag ref
		}
	}

	protected <BETWEEN> void init(int index) {
		context.register(this);
		if (parent != null)
			insertChild(index);
		for (Consumer<Context> binding : tag.getPreFixedBindings())
			binding.accept(context);
		assert (!context.containsProperty(tag, "filteredChildren"));
		FilteredChildren filteredChildren = new FilteredChildren();
		tag.createNewInitializedProperty("filteredChildren", context, c -> filteredChildren);
		for (Tag childTag : filteredChildren.filteredList)
			tagAdder.accept(childTag);
		filteredChildren.filteredList.addListener(tagListener);
		for (Consumer<Context> binding : tag.getPostFixedBindings())
			binding.accept(context);
	}

	private class FilteredChildren {
		final Map<Tag, ObservableValue<Boolean>[]> selectorsByTag = new HashMap<Tag, ObservableValue<Boolean>[]>();// Prevents garbage collection
		final ObservableList<Tag> filteredList = new FilteredList<Tag>(new ObservableListWrapperExtended<Tag>(context.getRootContext().getObservableChildren(tag), child -> {
			ObservableValue<Boolean>[] result = child.getSwitchers().stream().map(s -> s.apply(context, child)).toArray(ObservableValue[]::new);
			selectorsByTag.put(child, result);
			return result;
		}), child -> Arrays.stream(selectorsByTag.get(child)).allMatch(s -> Boolean.TRUE.equals(s.getValue())));
	}

	private int computeIndex(int indexInChildren, Tag childElement) {
		for (Tag child : context.getRootContext().getObservableChildren(tag)) {
			if (child == childElement)
				return indexInChildren;
			indexInChildren += sizeBySubTag.get(child);
		}
		return indexInChildren;
	}

	public Context getModelContext() {
		return context;
	}

	protected RootHtmlDomNode getRootHtmlDomNode() {
		return parent.getRootHtmlDomNode();
	}

	void insertChild(int index) {
		parent.incrementSize(tag);
		sendAdd(index);
		getRootHtmlDomNode().add(getId(), this);
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

	public Sender getSender() {
		return parent.getSender();
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
		// System.out.println(jsonObj.encodePrettily());
		sendMessage(jsonObj);
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
		getSender().send(jsonObj.encode());
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
		private final Sender send;
		private final String rootId;

		public RootHtmlDomNode(Context rootModelContext, RootTag rootTag, String rootId, Sender send) {
			super(null, rootModelContext, rootTag);
			this.rootId = rootId;
			this.send = send;
			sendAdd(0);
			init(0);
		}

		@Override
		public Sender getSender() {
			return send;
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