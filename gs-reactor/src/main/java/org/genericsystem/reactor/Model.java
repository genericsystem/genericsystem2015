package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.reactor.Tag.SelectableHtmlDomNode;

import javafx.beans.property.Property;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

/**
 * @author Nicolas Feybesse
 *
 */
public class Model {

	Model parent;
	private final Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private final Map<Tag<?>, List<? extends Model>> subContextsMap = new HashMap<>();

	public Model getParent() {
		return this.parent;
	}

	public List<? extends Model> getSubContexts(Tag<?> element) {
		return subContextsMap.get(element);
	}

	public List<Model> allSubContexts() {
		return subContextsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

	public <MODEL extends Model> void setSubContexts(Tag<?> element, List<MODEL> subContexts) {
		subContextsMap.put(element, subContexts);
	}

	public void register(ViewContext<?> viewContext) {
		ViewContext<?> previous = viewContextsMap.put(viewContext.getElement(), viewContext);
		assert previous == null;
	}

	public void destroy() {
		boolean first = true;
		for (ViewContext<?> viewContext : viewContextsMap.values()) {
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
