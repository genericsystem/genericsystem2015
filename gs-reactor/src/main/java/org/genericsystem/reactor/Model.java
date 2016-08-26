package org.genericsystem.reactor;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.defaults.tools.TransformationObservableList;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class Model {

	private Model parent;
	private Map<Tag<?>, HtmlDomNode<?>> htmlDomNodesMap = new LinkedHashMap<>();
	private Map<Tag<?>, ObservableList<Model>> subModelsMap = new HashMap<>();
	private Map<Tag<?>, Map<String, ObservableValue<?>>> propertiesMap = new HashMap<>();

	public Model(Model parent) {
		this.parent = parent;
	}

	public Model getParent() {
		return this.parent;
	}

	@SuppressWarnings("unchecked")
	public <SUBMODEL extends Model> ObservableList<SUBMODEL> getSubContexts(Tag<SUBMODEL> tag) {
		return (ObservableList<SUBMODEL>) subModelsMap.get(tag);
	}

	public boolean containsProperty(Tag<?> tag, String propertyName) {
		return propertiesMap.containsKey(tag) ? propertiesMap.get(tag).containsKey(propertyName) : false;
	}

	public void createNewProperty(Tag<?> tag, String propertyName) {
		assert htmlDomNodesMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to create an already used property : " + propertyName);
		getProperties(tag).put(propertyName, new SimpleObjectProperty<>());
	}

	private Map<String, ObservableValue<?>> getProperties(Tag<?> tag) {
		Map<String, ObservableValue<?>> properties = propertiesMap.get(tag);
		if (properties == null) {
			assert htmlDomNodesMap.keySet().contains(tag);
			propertiesMap.put(tag, properties = new HashMap<String, ObservableValue<?>>());
		}
		return properties;
	}

	@SuppressWarnings("unchecked")
	<T> ObservableValue<T> getObservableValue(Tag<?> tag, String propertyName) {
		return (ObservableValue<T>) getProperties(tag).get(propertyName);
	}

	@SuppressWarnings("unchecked")
	<T> Property<T> getProperty(Tag<?> tag, String propertyName) {
		return (Property<T>) getProperties(tag).get(propertyName);
	}

	public Collection<Map<String, ObservableValue<?>>> getPropertiesMaps() {
		return propertiesMap.values();
	}

	protected void storeProperty(Tag<?> tag, String propertyName, ObservableValue<?> value) {
		assert htmlDomNodesMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		getProperties(tag).put(propertyName, value);
	}

	public List<Model> subContexts() {
		return subModelsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	@SuppressWarnings("unchecked")
	<SUBMODEL extends Model> void setSubContexts(Tag<?> element, ObservableList<SUBMODEL> subContexts) {
		assert subModelsMap.get(element) == null;
		subModelsMap.put(element, (ObservableList<Model>) subContexts);
	}

	public void register(HtmlDomNode<?> htmlDomNode) {
		HtmlDomNode<?> previous = htmlDomNodesMap.put(htmlDomNode.getTag(), htmlDomNode);
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
		for (HtmlDomNode<?> htmlDomNode : htmlDomNodesMap.values()) {
			htmlDomNode.destroy();
		}
		for (ObservableList<Model> subModels : subModelsMap.values()) {
			((TransformationObservableList<?, ?>) subModels).unbind();
			for (Model subModel : subModels)
				subModel.internalDestroy();
		}
		subModelsMap = new HashMap<>();
		htmlDomNodesMap = new LinkedHashMap<>();
		propertiesMap = new HashMap<>();
	}

	@SuppressWarnings("unchecked")
	public <M extends Model> HtmlDomNode<M> getHtmlDomNode(Tag<?> element) {
		return (HtmlDomNode<M>) htmlDomNodesMap.get(element);
	}
}
