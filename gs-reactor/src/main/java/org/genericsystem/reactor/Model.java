package org.genericsystem.reactor;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag.SelectableHtmlDomNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class Model {

	protected Model parent;
	private Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private Map<Tag<?>, ObservableList<Model>> subModelsMap = new HashMap<>();
	private Map<Tag<?>, Map<String, ObservableValue<Object>>> propertiesMap = new HashMap<>();

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
		assert viewContextsMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to create an already used property : " + propertyName);
		getProperties(tag).put(propertyName, new SimpleObjectProperty<>());
	}

	private Map<String, ObservableValue<Object>> getProperties(Tag<?> tag) {
		Map<String, ObservableValue<Object>> properties = propertiesMap.get(tag);
		if (properties == null) {
			assert viewContextsMap.keySet().contains(tag);
			propertiesMap.put(tag, properties = new HashMap<String, ObservableValue<Object>>());
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

	public Collection<Map<String, ObservableValue<Object>>> getPropertiesMaps() {
		return propertiesMap.values();
	}

	public void storeProperty(Tag<?> tag, String propertyName, ObservableValue<?> value) {
		assert viewContextsMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		getProperties(tag).put(propertyName, (ObservableValue) value);
	}

	public List<Model> subContexts() {
		return subModelsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	@SuppressWarnings("unchecked")
	<SUBMODEL extends Model> void setSubContexts(Tag<?> element, ObservableList<SUBMODEL> subContexts) {
		assert subModelsMap.get(element) == null;
		subModelsMap.put(element, (ObservableList<Model>) subContexts);
	}

	public void register(ViewContext<?> viewContext) {
		ViewContext<?> previous = viewContextsMap.put(viewContext.getTag(), viewContext);
		assert previous == null;
	}

	public void destroy() {
		viewContextsMap.values().iterator().next().getNode().sendRemove();
		internalDestroy();
	}

	public boolean destroyed = false;

	public void internalDestroy() {
		// System.out.println("InternalDestroy : " + this);
		assert !destroyed;
		destroyed = true;
		for (ViewContext<?> viewContext : viewContextsMap.values()) {
			viewContext.destroy();
		}
		for (ObservableList<Model> subModels : subModelsMap.values()) {
			((TransformationObservableList<?, Model>) subModels).unbind();
			for (Model subModel : subModels)
				subModel.internalDestroy();
		}
		subModelsMap = new HashMap<>();
		viewContextsMap = new LinkedHashMap<>();
		propertiesMap = new HashMap<>();
	}

	public ViewContext<?> getViewContext(Tag<?> element) {
		return viewContextsMap.get(element);
	}

	public Property<String> getTextProperty(Tag<?> element) {
		return getViewContext(element).getNode().getTextProperty();
	}

	public ObservableSet<String> getObservableStyleClasses(Tag<?> element) {
		return getViewContext(element).getNode().getStyleClasses();
	}

	public ObservableMap<String, String> getObservableStyles(Tag<?> element) {
		return getViewContext(element).getNode().getStyles();
	}

	public ObservableMap<String, String> getObservableAttributes(Tag<?> element) {
		return getViewContext(element).getNode().getAttributes();
	}

	public Property<Number> getSelectionIndex(Tag<?> element) {
		return getViewContext(element).<SelectableHtmlDomNode> getNode().getSelectionIndex();
	}
}
