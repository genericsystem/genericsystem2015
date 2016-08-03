package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag.SelectableHtmlDomNode;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

/**
 * @author Nicolas Feybesse
 *
 */
public class Model {

	protected Model parent;
	private Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private Map<Tag<?>, ObservableList<Model>> subModelsMap = new HashMap<>();

	public Model getParent() {
		return this.parent;
	}

	public <SUBMODEL extends Model> ObservableList<SUBMODEL> getSubContexts(Tag<SUBMODEL> tag) {
		return (ObservableList<SUBMODEL>) subModelsMap.get(tag);
	}

	public List<Model> subContexts() {
		return subModelsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}

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
