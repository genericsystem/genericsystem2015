package org.genericsystem.reactor;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Tag.SelectableHtmlDomNode;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

/**
 * @author Nicolas Feybesse
 *
 */
public class Model {

	protected Model parent;
	private final Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private final Map<Tag<?>, List<? extends Model>> subContextsMap = new HashMap<>();
	private final Map<Tag<?>, Map<String, ObservableValue<Object>>> propertiesMap = new HashMap<Tag<?>, Map<String, ObservableValue<Object>>>() {
		@Override
		public Map<String, ObservableValue<Object>> get(Object key) {
			Map<String, ObservableValue<Object>> properties = super.get(key);
			if (properties == null)
				put((Tag) key, properties = new HashMap<String, ObservableValue<Object>>() {
					@Override
					public ObservableValue<Object> get(Object key) {
						ObservableValue<Object> property = super.get(key);
						if (property == null)
							put((String) key, property = new SimpleObjectProperty<>());
						return property;
					};
				});
			return properties;
		};
	};
	private TriFunction<Generic[], Serializable, Generic, Generic> action;

	@FunctionalInterface
	public interface TriFunction<T, U, R, S> {
		R apply(T t, U u, S s);
	}

	public Model getParent() {
		return this.parent;
	}

	public List<? extends Model> getSubContexts(Tag<?> tag) {
		return subContextsMap.get(tag);
	}

	public <T> ObservableValue<T> getObservableValue(Tag<?> tag, String name) {
		return (ObservableValue<T>) propertiesMap.get(tag).get(name);
	}

	public <T> Property<T> getProperty(Tag<?> tag, String name) {
		return (Property<T>) propertiesMap.get(tag).get(name);
	}

	public List<ObservableValue> getPropertiesByName(String propertyName) {
		return propertiesMap.values().stream().map(map -> map.get(propertyName)).filter(property -> property != null).collect(Collectors.toList());
	}

	public void setProperty(Tag tag, String propertyName, ObservableValue value) {
		propertiesMap.get(tag).put(propertyName, value);
	}

	public TriFunction<Generic[], Serializable, Generic, Generic> getAction() {
		return action;
	}

	public void setAction(TriFunction<Generic[], Serializable, Generic, Generic> action) {
		this.action = action;
	}

	public List<Model> allSubContexts() {
		return subContextsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	public <MODEL extends Model> void setSubContexts(Tag<?> element, List<MODEL> subContexts) {
		assert subContextsMap.get(element) == null;
		subContextsMap.put(element, subContexts);
	}

	public void register(ViewContext<?> viewContext) {
		ViewContext<?> previous = viewContextsMap.put(viewContext.getTag(), viewContext);
		assert previous == null;
	}

	public void destroy() {
		boolean first = true;
		for (ViewContext<?> viewContext : viewContextsMap.values()) {
			assert !viewContext.removed;
			viewContext.destroyChild();
			if (first) {
				viewContext.getNode().sendRemove();
				first = false;
			}
		}
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
