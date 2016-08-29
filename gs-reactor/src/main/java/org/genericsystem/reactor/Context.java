package org.genericsystem.reactor;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class Context {
	protected static Logger log = LoggerFactory.getLogger(Context.class);
	private Context parent;
	private Map<Tag, HtmlDomNode> htmlDomNodesMap = new LinkedHashMap<>();
	private Map<Tag, ObservableList<Context>> subModelsMap = new HashMap<>();
	private Map<Tag, Map<String, ObservableValue<?>>> propertiesMap = new HashMap<>();
	private final Generic[] generics;

	public Context(Context parent, Generic[] generics) {
		this.parent = parent;
		this.generics = generics;
	}

	public Context getParent() {
		return this.parent;
	}

	public ObservableList<Context> getSubContexts(Tag tag) {
		return subModelsMap.get(tag);
	}

	public boolean containsProperty(Tag tag, String propertyName) {
		return propertiesMap.containsKey(tag) ? propertiesMap.get(tag).containsKey(propertyName) : false;
	}

	public void createNewProperty(Tag tag, String propertyName) {
		assert htmlDomNodesMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to create an already used property : " + propertyName);
		getProperties(tag).put(propertyName, new SimpleObjectProperty<>());
	}

	private Map<String, ObservableValue<?>> getProperties(Tag tag) {
		Map<String, ObservableValue<?>> properties = propertiesMap.get(tag);
		if (properties == null) {
			assert htmlDomNodesMap.keySet().contains(tag);
			propertiesMap.put(tag, properties = new HashMap<String, ObservableValue<?>>());
		}
		return properties;
	}

	@SuppressWarnings("unchecked")
	<T> ObservableValue<T> getObservableValue(Tag tag, String propertyName) {
		return (ObservableValue<T>) getProperties(tag).get(propertyName);
	}

	@SuppressWarnings("unchecked")
	<T> Property<T> getProperty(Tag tag, String propertyName) {
		return (Property<T>) getProperties(tag).get(propertyName);
	}

	public Collection<Map<String, ObservableValue<?>>> getPropertiesMaps() {
		return propertiesMap.values();
	}

	protected void storeProperty(Tag tag, String propertyName, ObservableValue<?> value) {
		assert htmlDomNodesMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		getProperties(tag).put(propertyName, value);
	}

	public void removeSubContexts(Tag tag) {
		subModelsMap.remove(tag);
	}

	void setSubContexts(Tag tag, ObservableList<Context> subContexts) {
		assert subModelsMap.get(tag) == null;
		subModelsMap.put(tag, subContexts);
	}

	public void register(HtmlDomNode htmlDomNode) {
		HtmlDomNode previous = htmlDomNodesMap.put(htmlDomNode.getTag(), htmlDomNode);
		assert previous == null;
	}

	public void destroy() {
		htmlDomNodesMap.values().iterator().next().sendRemove();
		internalDestroy();
	}

	public boolean destroyed = false;

	public void internalDestroy() {
		// System.out.println("InternalDestroy : " + this);
		assert !destroyed;
		destroyed = true;
		for (HtmlDomNode htmlDomNode : htmlDomNodesMap.values()) {
			htmlDomNode.destroy();
		}
		for (ObservableList<Context> subModels : subModelsMap.values()) {
			((TransformationObservableList<?, ?>) subModels).unbind();
			for (Context subModel : subModels)
				subModel.internalDestroy();
		}
		subModelsMap = new HashMap<>();
		htmlDomNodesMap = new LinkedHashMap<>();
		propertiesMap = new HashMap<>();
	}

	public HtmlDomNode getHtmlDomNode(Tag element) {
		return htmlDomNodesMap.get(element);
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
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

}
