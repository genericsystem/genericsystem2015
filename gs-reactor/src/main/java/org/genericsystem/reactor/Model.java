package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
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
	private final Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private Map<Tag<?>, TransformationObservableList<?, Model>> subContextsMap = new HashMap<>();
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

	@FunctionalInterface
	public interface TriFunction<T, U, R, S> {
		R apply(T t, U u, S s);
	}

	public Model getParent() {
		return this.parent;
	}

	public <SUBMODEL extends Model> List<SUBMODEL> getSubContexts(Tag<SUBMODEL> tag) {
		return (List<SUBMODEL>) subContextsMap.get(tag);
	}

	public Map<Tag<?>, Map<String, ObservableValue<Object>>> getProperties() {
		return propertiesMap;
	}

	public <T> ObservableValue<T> getObservableValue(Tag<?> tag, String name) {
		return (ObservableValue<T>) propertiesMap.get(tag).get(name);
	}

	public <T> Property<T> getProperty(Tag<?> tag, String name) {
		return (Property<T>) propertiesMap.get(tag).get(name);
	}

	public void storeProperty(Tag tag, String propertyName, ObservableValue value) {
		if (propertiesMap.get(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		propertiesMap.get(tag).put(propertyName, value);
	}

	public List<Model> allSubContexts() {
		return subContextsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	public <MODEL extends Model> void setSubContexts(Tag<?> element, TransformationObservableList<?, MODEL> subContexts) {
		assert subContextsMap.get(element) == null;
		subContextsMap.put(element, (TransformationObservableList) subContexts);
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
		for (TransformationObservableList<?, Model> subModels : subContextsMap.values()) {
			subModels.unbind();
			for (Model subModel : subModels)
				subModel.internalDestroy();
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
