package org.genericsystem.reactor;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.context.RootContext;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableLongValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class Context {
	protected static Logger log = LoggerFactory.getLogger(Context.class);
	private Context parent;
	private Map<Tag, HtmlDomNode> htmlDomNodesMap = new LinkedHashMap<>();
	private Map<Tag, ObservableList<Context>> subContextsMap = new HashMap<>();
	private Map<Tag, Map<String, ObservableValue<?>>> propertiesMap = new HashMap<>();
	private final Generic[] generics;
	private boolean destroyed = false;

	public Context(Context parent, Generic[] generics) {
		this.parent = parent;
		this.generics = generics;
	}

	public Map<Tag, HtmlDomNode> getHtmlDomNodesMap() {
		return htmlDomNodesMap;
	}

	public Context getParent() {
		return parent;
	}

	public ObservableList<Context> getSubContexts(Tag tag) {
		return subContextsMap.get(tag);
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

	public Map<String, ObservableValue<?>> getPropertiesMaps(Tag tag) {
		return propertiesMap.get(tag);
	}

	protected void storeProperty(Tag tag, String propertyName, ObservableValue<?> value) {
		assert htmlDomNodesMap.keySet().contains(tag);
		if (getProperties(tag).containsKey(propertyName))
			throw new IllegalStateException("Unable to store an already used property : " + propertyName);
		getProperties(tag).put(propertyName, value);
	}

	public void removeSubContexts(Tag tag) {
		subContextsMap.remove(tag);
	}

	public void removeProperties(Tag tag) {
		propertiesMap.remove(tag);
	}

	public void removeHtmlDomNode(Tag tag) {
		htmlDomNodesMap.remove(tag);
	}

	void setSubContexts(Tag tag, ObservableList<Context> subContexts) {
		assert subContextsMap.get(tag) == null;
		subContextsMap.put(tag, subContexts);
	}

	public void register(HtmlDomNode htmlDomNode) {
		HtmlDomNode previous = htmlDomNodesMap.put(htmlDomNode.getTag(), htmlDomNode);
		assert previous == null;
	}

	public void destroy() {
		// System.out.println("context destroy : " + this);
		assert !destroyed;
		destroyed = true;
		htmlDomNodesMap.values().stream().findFirst().get().destroy();
		for (ObservableList<Context> subModels : subContextsMap.values()) {
			((TransformationObservableList<?, ?>) subModels).unbind();
			for (Context subModel : subModels)
				subModel.destroy();
		}
		subContextsMap = new HashMap<>();
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
		traverse();
		getGeneric().getCurrentCache().flush();
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
	}

	public boolean isOpaque() {
		// System.out.println(getGeneric().info() + getGeneric().getCurrentCache().contains(getGeneric()));
		return getGeneric().getCurrentCache().contains(getGeneric());
	}

	public long shiftTs() throws RollbackException {
		return getGeneric().getCurrentCache().shiftTs();
	}

	public void traverse() {
		if (isOpaque())
			htmlDomNodesMap.keySet().stream().forEach(tag -> tag.removeStyleClass(this, "opaque"));
		subContextsMap.values().stream().flatMap(c -> c.stream()).forEach(Context::traverse);
	}

	public boolean isDestroyed() {
		return destroyed;
	}

	public RootContext getRootContext() {
		return getParent().getRootContext();
	}
}
