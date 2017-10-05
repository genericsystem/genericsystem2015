package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.genericsystem.reactor.FilteredChildren.FilteredTagChildren;
import org.genericsystem.reactor.MetaBinding.IndexedSubContext;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.Observable;
import io.reactivex.disposables.CompositeDisposable;
import io.vertx.core.json.JsonObject;
import javafx.beans.binding.BooleanExpression;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.SetChangeListener;

public class HtmlDomNode {

	private static final Logger logger = LoggerFactory.getLogger(HtmlDomNode.class);
	static int count = 0;
	private final String id;
	private HtmlDomNode parent;
	private Tag tag;
	private Context context;
	private boolean destroyed = false;
	final Consumer<Tag> tagAdder = tagAdder();
	private final CompositeDisposable disposables = new CompositeDisposable();
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
			if (change.wasRemoved())
				change.getRemoved().forEach(childTag -> context.removeTag(childTag));
			if (change.wasAdded())
				change.getAddedSubList().forEach(tagAdder::accept);
		}
	};

	public HtmlDomNode(HtmlDomNode parent, Context context, Tag tag) {
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.parent = parent;
		this.tag = tag;
		this.context = context;
		tag.getRootTag().initDomNode(this);
	}

	protected <BETWEEN> void init(int index) {
		context.register(this);
		if (parent != null)
			insertChild(index);

		// Add the “opaque” class if the generic contained in the context is in the cache (unsaved).
		if (parent != null) {
			BooleanExpression parentInCache = parent.getModelContext().isInCache();
			BooleanExpression inCache = context.isInCache();
			tag.bindOptionalStyleClass("opaque", "isInCache", context, inCache.and(parentInCache.not()));
		}

		for (Consumer<Context> binding : tag.getPreFixedBindings())
			context.getCache().safeExecute(() -> binding.accept(context));
		assert (!context.containsAttribute(tag, "filteredChildren"));
		FilteredTagChildren filteredChildren = new FilteredTagChildren(tag, context);
		tag.addContextAttribute("filteredChildren", context, filteredChildren);
		for (Tag childTag : filteredChildren.filteredList)
			tagAdder.accept(childTag);
		filteredChildren.filteredList.addListener(tagListener);
		for (Consumer<Context> binding : tag.getPostFixedBindings())
			context.getCache().safeExecute(() -> binding.accept(context));
	}

	void destroy() {
		// System.out.println("Attempt to destroy : " + getId());
		assert !destroyed : "Node : " + getId();
		destroyed = true;
		((FilteredTagChildren) tag.getContextAttribute("filteredChildren", context)).filteredList.removeListener(tagListener);
		for (Tag childTag : tag.getObservableChildren())
			childTag.getMetaBindingProperty().removeListener(metaBindingListeners.get(childTag));
		disposables.dispose();
		tag.getDomNodeTextProperty(context).removeListener(textListener);
		tag.getDomNodeStyles(context).removeListener(stylesListener);
		tag.getDomNodeAttributes(context).removeListener(attributesListener);
		tag.getDomNodeStyleClasses(context).removeListener(styleClassesListener);
		getRootHtmlDomNode().remove(getId());
		if (parent != null)
			parent.decrementSize(tag);
	}

	public static interface Sender {
		public void send(String message);
	}

	public CompositeDisposable getDisposables() {
		return disposables;
	}

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

	private <BETWEEN> Consumer<Tag> tagAdder() {
		return childTag -> {
			Property<MetaBinding<BETWEEN>> metaBinding = childTag.getMetaBindingProperty();
			metaBinding.addListener(metaBindingListeners.get(childTag));
			updateMetaBinding(childTag, metaBinding.getValue());
		};
	}

	private <BETWEEN> void updateMetaBinding(Tag childTag, MetaBinding<BETWEEN> metaBinding) {
		if (metaBinding != null) {
			if (context.getSubContexts(childTag) == null) {
				context.setSubContexts(childTag, FXCollections.observableArrayList());
				Observable<IndexedSubContext> subContexts = metaBinding.buildFilteredChildren(context, childTag);
				// TODO: Dispose on MetaBinding change.
				disposables.add(subContexts.subscribe(sc -> {
					Context subContext = sc.getContext();
					if (sc.getCreate()) {
						if (context.addSubContext(childTag, subContext))
							childTag.createNode(this, subContext).init(computeIndex(sc.getIndex(), childTag));
					} else {
						subContext.removeTag(childTag);
						if (sc.getIndex() == -1) {// SubContext removed
							context.getSubContexts(childTag).forEach(c_ -> {
								if (c_.equals(subContext))
									c_.destroy();
							});
						}
						context.getSubContexts(childTag).remove(subContext);
					}
				}, e -> logger.error("Error on filtered contexts subscriber.", e)));
			}
		} else if (context.getHtmlDomNode(childTag) == null)
			childTag.createNode(this, context).init(computeIndex(0, childTag));
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
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.REMOVE_STYLE).put(ReactorStatics.ID, getId()).put(ReactorStatics.STYLE_PROPERTY, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.ADD_STYLE).put(ReactorStatics.ID, getId()).put(ReactorStatics.STYLE_PROPERTY, change.getKey()).put(ReactorStatics.STYLE_VALUE, change.getValueAdded()));
	};

	private final MapChangeListener<String, String> attributesListener = change -> {
		if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.REMOVE_ATTRIBUTE).put(ReactorStatics.ID, getId()).put(ReactorStatics.ATTRIBUTE_NAME, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.ADD_ATTRIBUTE).put(ReactorStatics.ID, getId()).put(ReactorStatics.ATTRIBUTE_NAME, change.getKey()).put(ReactorStatics.ATTRIBUTE_VALUE, change.getValueAdded()));
	};

	private final SetChangeListener<String> styleClassesListener = change -> {
		if (change.wasAdded())
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.ADD_STYLECLASS).put(ReactorStatics.ID, getId()).put(ReactorStatics.STYLECLASS, change.getElementAdded()));
		else
			sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.REMOVE_STYLECLASS).put(ReactorStatics.ID, getId()).put(ReactorStatics.STYLECLASS, change.getElementRemoved()));
	};

	private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.UPDATE_TEXT).put(ReactorStatics.ID, getId()).put(ReactorStatics.TEXT_CONTENT, newValue != null ? newValue : ""));

	private final Map<Tag, ChangeListener<MetaBinding<?>>> metaBindingListeners = new HashMap<Tag, ChangeListener<MetaBinding<?>>>() {

		private static final long serialVersionUID = 6179552869588758790L;

		@Override
		public ChangeListener<MetaBinding<?>> get(Object key) {
			ChangeListener<MetaBinding<?>> listener = super.get(key);
			if (listener == null && key instanceof Tag) {
				Tag childTag = (Tag) key;
				put(childTag, listener = (o, ov, nv) -> {
					context.removeTag(childTag);
					updateMetaBinding(childTag, nv);
				});
			}
			return listener;
		}
	};

	private final ChangeListener<Number> indexListener = (o, old, newValue) -> {
		// System.out.println(new JsonObject().put(MSG_TYPE,
		// UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue !=
		// null ? newValue : 0)
		// .encodePrettily());
		sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.UPDATE_SELECTION).put(ReactorStatics.ID, getId()).put(ReactorStatics.SELECTED_INDEX, newValue != null ? newValue : 0));
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
		JsonObject jsonObj = new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.ADD);
		jsonObj.put(ReactorStatics.PARENT_ID, getParentId());
		jsonObj.put(ReactorStatics.ID, id);
		jsonObj.put(ReactorStatics.TAG_HTML, getTag().getTag());
		jsonObj.put(ReactorStatics.NEXT_ID, index);
		// System.out.println(jsonObj.encodePrettily());
		sendMessage(jsonObj);
	}

	public void sendRemove() {
		sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.REMOVE).put(ReactorStatics.ID, id));
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

	public String toHTMLString() {
		String tagText = tag.getTag();
		String classes = tag.getDomNodeStyleClasses(context).stream().collect(Collectors.joining(" "));
		classes = ("section".equals(tagText) || "div".equals(tagText) || "header".equals(tagText) || "footer".equals(tagText)) ? classes += " adding" : classes;

		String styles = tag.getDomNodeStyles(context).entrySet().stream().map(m -> m.getKey() + ": " + m.getValue()).collect(Collectors.joining("; "));
		String tagAttributes = tag.getDomNodeAttributes(context).entrySet().stream().filter(m -> m.getValue() != null && !m.getValue().isEmpty()).map(m -> m.getKey() + "=\"" + m.getValue() + "\"").collect(Collectors.joining(""));

		String body = "\n<" + tagText + " id=\"" + id + "\"";
		if (!classes.equals(""))
			body += " class=\"" + classes + "\"";
		if (!styles.equals(""))
			body += " style=\"" + styles + "\"";

		body += tagAttributes + ">";

		for (HtmlDomNode node : getChildren())
			body += node.toHTMLString();
		String tagValue = tag.getDomNodeTextProperty(context).getValue();
		if (tagValue != null)
			body += tagValue;

		body += "</" + tagText + ">\n";
		return body;
	}

	public static class HtmlDomNodeAction extends HtmlDomNode {

		public HtmlDomNodeAction(HtmlDomNode parent, Context context, Tag tag) {
			super(parent, context, tag);
		}

		@Override
		public void handleMessage(JsonObject json) {
			((ActionDefaults) getTag()).getActionProperty(getModelContext()).getValue().accept(new Object());
		}
	}

	public static class HtmlDomNodeCheckbox extends HtmlDomNode {

		public HtmlDomNodeCheckbox(HtmlDomNode parent, Context context, Tag tag) {
			super(parent, context, tag);
		}

		@Override
		public void handleMessage(JsonObject json) {
			getTag().getDomNodeAttributes(getModelContext()).put(ReactorStatics.CHECKED, json.getBoolean(ReactorStatics.CHECKED) ? ReactorStatics.CHECKED : "");
		}
	}

	public static class HtmlDomNodeInputText extends HtmlDomNode {

		public HtmlDomNodeInputText(HtmlDomNode parent, Context context, Tag tag) {
			super(parent, context, tag);
		}

		@Override
		public void handleMessage(JsonObject json) {
			super.handleMessage(json);
			if (ReactorStatics.ADD.equals(json.getString(ReactorStatics.MSG_TYPE))) {
				Property<Consumer<Object>> action = ((ActionDefaults) getTag()).getActionProperty(getModelContext());
				if (action != null)
					action.getValue().accept(new Object());
			}
			if (ReactorStatics.UPDATE.equals(json.getString(ReactorStatics.MSG_TYPE)))
				getTag().getDomNodeAttributes(getModelContext()).put("value", json.getString(ReactorStatics.TEXT_CONTENT));
		}
	}

	public static class HtmlDomNodeSelect extends HtmlDomNode {

		public HtmlDomNodeSelect(HtmlDomNode parent, Context context, Tag tag) {
			super(parent, context, tag);
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ReactorStatics.UPDATE.equals(json.getString(ReactorStatics.MSG_TYPE))) {
				((SelectionDefaults) getTag()).getSelectionIndex(getModelContext()).setValue(json.getInteger(ReactorStatics.SELECTED_INDEX));
			}
		}
	}
}