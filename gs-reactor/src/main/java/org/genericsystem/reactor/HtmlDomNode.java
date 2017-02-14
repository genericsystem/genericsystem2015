package org.genericsystem.reactor;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;

import com.sun.javafx.collections.ObservableListWrapper;

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
	private final String id;
	private HtmlDomNode parent;
	private Tag tag;
	private Context context;

	private boolean destroyed = false;

	public static interface Sender {
		public void send(String message);
	}

	final Consumer<Tag> tagAdder = tagAdder();
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
			metaBinding.addListener(metaBindingListeners.get(childTag));
			updateMetaBinding(childTag, metaBinding.getValue());
		};
	}

	private <BETWEEN> void updateMetaBinding(Tag childTag, MetaBinding<BETWEEN> metaBinding) {
		if (metaBinding != null) {
			if (context.getSubContexts(childTag) == null) {
				FilteredChildContexts<BETWEEN> subContexts = new FilteredChildContexts<>(metaBinding, childTag);
				childTag.addContextAttribute("filteredContexts", context, subContexts);
				context.setSubContexts(childTag, new TransformationObservableList<Context, Context>(subContexts.filteredSubContexts, (i, subContext) -> {
					childTag.createNode(this, subContext).init(computeIndex(i, childTag));
					if (subContext.isInCache())
						childTag.addStyleClass(subContext, "opaque");
					return subContext;
				}, subContext -> subContext.removeTag(childTag)));
			}
		} else if (context.getHtmlDomNode(childTag) == null)
			childTag.createNode(this, context).init(computeIndex(0, childTag));
	}

	void destroy() {
		// System.out.println("Attempt to destroy : " + getId());
		assert !destroyed : "Node : " + getId();
		destroyed = true;
		((FilteredTagChildren) tag.getContextAttribute("filteredChildren", context)).filteredList.removeListener(tagListener);
		for (Tag childTag : tag.getObservableChildren()) {
			childTag.getMetaBindingProperty().removeListener(metaBindingListeners.get(childTag));
			if (childTag.getMetaBinding() != null && context.getSubContexts(childTag) != null)
				((FilteredChildContexts<?>) childTag.getContextAttribute("filteredContexts", context)).transformationListSubContexts.unbind();
		}
		tag.getDomNodeTextProperty(context).removeListener(textListener);
		tag.getDomNodeStyles(context).removeListener(stylesListener);
		tag.getDomNodeAttributes(context).removeListener(attributesListener);
		tag.getDomNodeStyleClasses(context).removeListener(styleClassesListener);
		getRootHtmlDomNode().remove(getId());
		parent.decrementSize(tag);
	}

	protected <BETWEEN> void init(int index) {
		context.register(this);
		if (parent != null)
			insertChild(index);
		for (Consumer<Context> binding : tag.getPreFixedBindings())
			binding.accept(context);
		assert (!context.containsAttribute(tag, "filteredChildren"));
		FilteredTagChildren filteredChildren = new FilteredTagChildren();
		tag.addContextAttribute("filteredChildren", context, filteredChildren);
		for (Tag childTag : filteredChildren.filteredList)
			tagAdder.accept(childTag);
		filteredChildren.filteredList.addListener(tagListener);
		for (Consumer<Context> binding : tag.getPostFixedBindings())
			binding.accept(context);
	}

	private class FilteredChildren<T> {
		final Map<T, ObservableList<TagSwitcher>> selectorsByChild = new HashMap<>();// Prevents garbage collection
		final Map<T, Map<TagSwitcher, ObservableValue<Boolean>>> selectorsByChildAndSwitcher = new HashMap<T, Map<TagSwitcher, ObservableValue<Boolean>>>() {

			private static final long serialVersionUID = -5831485781427983238L;

			@Override
			public Map<TagSwitcher, ObservableValue<Boolean>> get(Object key) {
				Map<TagSwitcher, ObservableValue<Boolean>> result = super.get(key);
				if (result == null)
					put((T) key, result = new HashMap<>());
				return result;
			}
		};
	}

	private class FilteredTagChildren extends FilteredChildren<Tag> {

		final ObservableList<Tag> filteredList = new FilteredList<>(new ObservableListWrapper<>(context.getRootContext().getObservableChildren(tag), child -> {
			if (child.getMetaBinding() != null)
				return new ObservableValue[] {};
			ObservableList<TagSwitcher> result = new ObservableListWrapper<>(child.getObservableSwitchers(), s -> {
				ObservableValue<Boolean> selector = s.apply(context, child);
				selectorsByChildAndSwitcher.get(child).put(s, selector);
				return new ObservableValue[] { selector };
			});
			selectorsByChild.put(child, result);
			return new ObservableList[] { result };
		}), child -> child.getMetaBinding() != null || selectorsByChildAndSwitcher.get(child).entrySet().stream().allMatch(entry -> !selectorsByChild.get(child).contains(entry.getKey()) || Boolean.TRUE.equals(entry.getValue().getValue())));
	}

	class FilteredChildContexts<BETWEEN> extends FilteredChildren<Context> {
		final ObservableList<Context> filteredSubContexts;
		final TransformationObservableList<BETWEEN, Context> transformationListSubContexts;

		FilteredChildContexts(MetaBinding<BETWEEN> metaBinding, Tag childTag) {
			transformationListSubContexts = new TransformationObservableList<>(metaBinding.buildBetweenChildren(context), (i, between) -> metaBinding.buildModel(context, between), Context::destroy, childContext -> {
				ObservableList<TagSwitcher> result = new ObservableListWrapper<>(childTag.getObservableSwitchers(), s -> {
					ObservableValue<Boolean> selector = s.apply(childContext, childTag);
					selectorsByChildAndSwitcher.get(childContext).put(s, selector);
					return new ObservableValue[] { selector };
				});
				selectorsByChild.put(childContext, result);
				return new ObservableList[] { result };
			});
			filteredSubContexts = new FilteredList<>(transformationListSubContexts,
					childContext -> selectorsByChildAndSwitcher.get(childContext).entrySet().stream().allMatch(entry -> !selectorsByChild.get(childContext).contains(entry.getKey()) || Boolean.TRUE.equals(entry.getValue().getValue())));
		}
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

	private final ChangeListener<String> textListener = (o, old,
			newValue) -> sendMessage(new JsonObject().put(ReactorStatics.MSG_TYPE, ReactorStatics.UPDATE_TEXT).put(ReactorStatics.ID, getId()).put(ReactorStatics.TEXT_CONTENT, newValue != null ? newValue : ""));

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