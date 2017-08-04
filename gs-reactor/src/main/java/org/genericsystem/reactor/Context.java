package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.FilteredChildren.FilteredChildContexts;
import org.genericsystem.reactor.context.RootContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableLongValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class Context {
	protected static Logger log = LoggerFactory.getLogger(Context.class);
	private Context parent;
	private Map<Tag, TagData> tagDataMap = new LinkedHashMap<>();
	private final Generic[] generics;
	private boolean destroyed = false;

	private static class TagData {
		private HtmlDomNode htmlDomNode;
		private ObservableList<Context> subContexts;
		private final Map<String, Object> attributes = new HashMap<>();

		public HtmlDomNode getHtmlDomNode() {
			return htmlDomNode;
		}

		public ObservableList<Context> getSubContexts() {
			return subContexts;
		}

		public Map<String, Object> getAttributes() {
			return attributes;
		}

		public void setHtmlDomNode(HtmlDomNode htmlDomNode) {
			this.htmlDomNode = htmlDomNode;
		}

		public void setSubContexts(ObservableList<Context> subContexts) {
			this.subContexts = subContexts;
		}
	}

	public Context(Context parent, Generic[] generics) {
		this.parent = parent;
		this.generics = generics;
	}

	public Context getParent() {
		return parent;
	}

	public ObservableList<Context> getSubContexts(Tag tag) {
		return tagDataMap.get(tag) != null ? tagDataMap.get(tag).getSubContexts() : null;
	}

	public List<Context> getSubContexts() {
		return tagDataMap.values().stream().map(tagData -> tagData.getSubContexts()).filter(list -> list != null).flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	public List<ObservableList<Context>> getSubContextsLists() {
		return tagDataMap.values().stream().map(tagData -> tagData.getSubContexts()).filter(list -> list != null).collect(Collectors.toList());
	}

	public boolean containsAttribute(Tag tag, String propertyName) {
		return tagDataMap.containsKey(tag) ? tagDataMap.get(tag).getAttributes().containsKey(propertyName) : false;
	}

	public Map<String, Object> getAttributes(Tag tag) {
		TagData tagData = tagDataMap.get(tag);
		if (tagData == null)
			tagDataMap.put(tag, tagData = new TagData());
		return tagData.getAttributes();
	}

	@SuppressWarnings("unchecked")
	<T> T getAttribute(Tag tag, String propertyName) {
		return (T) getAttributes(tag).get(propertyName);
	}

	public void createNewContextProperty(Tag tag, String propertyName) {
		if (getAttributes(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to create an already used property : " + propertyName);
		getAttributes(tag).put(propertyName, new SimpleObjectProperty<>());
	}

	@SuppressWarnings("unchecked")
	protected <T> void setContextPropertyValue(Tag tag, String propertyName, T value) {
		if (!containsAttribute(tag, propertyName))
			createNewContextProperty(tag, propertyName);
		((Property<T>) getAttribute(tag, propertyName)).setValue(value);
	}

	protected <T> void addContextAttribute(Tag tag, String propertyName, T value) {
		if (getAttributes(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		getAttributes(tag).put(propertyName, value);
	}

	protected <T> void setContextAttribute(Tag tag, String propertyName, T value) {
		getAttributes(tag).put(propertyName, value);
	}

	public void removeTag(Tag tag) {
		HtmlDomNode htmlDomNode = getHtmlDomNode(tag);
		if (htmlDomNode != null) {
			for (Tag childTag : tag.getObservableChildren())
				removeTag(childTag);
			htmlDomNode.destroy();
			htmlDomNode.sendRemove();
		}
		if (getSubContexts(tag) != null) {
			((TransformationObservableList<?, ?>) getSubContexts(tag)).unbind();
			((FilteredChildContexts<?>) tag.getContextAttribute("filteredContexts", this)).transformationListSubContexts.unbind();
			for (Context subContext : getSubContexts(tag))
				subContext.destroy();
		}
		tagDataMap.remove(tag);
	}

	public void addTag(Tag tag) {
		getHtmlDomNode(tag.getParent()).tagAdder.accept(tag);
	}

	void setSubContexts(Tag tag, ObservableList<Context> subContexts) {
		TagData tagData = tagDataMap.get(tag);
		assert tagData == null || tagData.getSubContexts() == null;
		if (tagData == null)
			tagDataMap.put(tag, tagData = new TagData());
		tagData.setSubContexts(subContexts);
	}

	public void register(HtmlDomNode htmlDomNode) {
		TagData domNodeTagData = tagDataMap.get(htmlDomNode.getTag());
		assert domNodeTagData == null || domNodeTagData.getHtmlDomNode() == null;
		if (domNodeTagData == null)
			tagDataMap.put(htmlDomNode.getTag(), domNodeTagData = new TagData());
		domNodeTagData.setHtmlDomNode(htmlDomNode);
	}

	public void destroy() {
		// System.out.println("context destroy : " + this);
		assert !destroyed : this;
		destroyed = true;
		for (ObservableList<Context> subModels : getSubContextsLists()) {
			((TransformationObservableList<?, ?>) subModels).unbind();
			for (Context subModel : subModels)
				subModel.destroy();
		}
		List<HtmlDomNode> domNodes = getHtmlDomNodes();
		domNodes.forEach(htmlDomNode -> htmlDomNode.destroy());
		if (!domNodes.isEmpty())
			domNodes.get(0).sendRemove();
		tagDataMap = new LinkedHashMap<>();
	}

	public HtmlDomNode getHtmlDomNode(Tag tag) {
		return tagDataMap.get(tag) != null ? tagDataMap.get(tag).getHtmlDomNode() : null;
	}

	public List<HtmlDomNode> getHtmlDomNodes() {
		return tagDataMap.values().stream().map(tagData -> tagData.getHtmlDomNode()).filter(htmlDomNode -> htmlDomNode != null).collect(Collectors.toList());
	}

	public Map<Tag, TagData> getTagDataMap() {
		return tagDataMap;
	}

	public Generic[] getGenerics() {
		return generics;
	}

	public static Generic[] addToGenerics(Generic generic, Generic[] generics) {
		if (generics.length != 0 && generics[0].equals(generic))
			return generics;
		Generic[] result = new Generic[generics.length + 1];
		result[0] = generic;
		System.arraycopy(generics, 0, result, 1, generics.length);
		return result;
	}

	public Generic getGeneric() {
		return generics[0];
	}

	public void remove() {
		getGeneric().remove();
	}

	public Generic find(Class<?> clazz) {
		return getGeneric().getRoot().find(clazz);
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
		traverse();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	public ObservableIntegerValue getCacheLevelObservableValue() {
		return getGeneric().getCurrentCache().getCacheLevelObservableValue();
	}

	public ObservableLongValue getTsObservableValue() {
		return getGeneric().getCurrentCache().getTsObservableValue();
	}

	public void mount() {
		getGeneric().getCurrentCache().mount();
	}

	public void unmount() {
		getGeneric().getCurrentCache().unmount();
		traverse();
	}

	public boolean isInCache() {
		// System.out.println(getGeneric().info() + getGeneric().getCurrentCache().contains(getGeneric()));
		return getGeneric().getCurrentCache().contains(getGeneric());
	}

	public long shiftTs() throws RollbackException {
		return getGeneric().getCurrentCache().shiftTs();
	}

	public void traverse() {
		Stream<Tag> tagsWithDomNode = tagDataMap.entrySet().stream().filter(entry -> entry.getValue().getHtmlDomNode() != null).map(entry -> entry.getKey());
		if (isInCache())
			tagsWithDomNode.forEach(tag -> tag.addStyleClass(this, "opaque"));
		else
			tagsWithDomNode.forEach(tag -> tag.removeStyleClass(this, "opaque"));
		getSubContexts().forEach(Context::traverse);
	}

	public boolean isDestroyed() {
		return destroyed;
	}

	public RootContext getRootContext() {
		return getParent().getRootContext();
	}
}
