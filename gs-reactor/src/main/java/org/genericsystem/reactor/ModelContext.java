package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag.ModelConstructor;
import org.genericsystem.reactor.Tag.SelectableHtmlDomNode;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

/**
 * @author Nicolas Feybesse
 *
 */
public class ModelContext {

	private final ModelContext parent;
	private final Model model;
	private final Map<Tag<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private final Map<Tag<?>, List<ModelContext>> subContextsMap = new HashMap<>();
	private final Map<Tag<?>, ObservableList<? extends Model>> observableSubModels = new HashMap<>();

	private ModelContext(ModelContext parent, Model model) {
		this.parent = parent;
		this.model = model;
	}

	public ModelContext createChildContext(Model childModel, ViewContext<?> viewContext, int index, Tag<?> childElement) {
		childModel.parent = getModel();// inject parent
		childModel.afterParentConstruct();
		ModelContext modelContextChild = new ModelContext(this, childModel);
		childModel.modelContext = modelContextChild;
		viewContext.createViewContextChild(index, modelContextChild, childElement);
		return modelContextChild;
	}

	@Override
	public String toString() {
		return "ModelContext : " + model;
	}

	@SuppressWarnings("unchecked")
	public <M extends Model> M getModel() {
		return (M) model;
	}

	public ModelContext getParent() {
		return this.parent;
	}

	public List<ModelContext> getSubContexts(Tag<?> element) {
		return subContextsMap.get(element);
	}
	
	public List<ModelContext> allSubContexts() {
		return subContextsMap.values().stream().flatMap(list -> list.stream()).collect(Collectors.toList());
	}
	
	public <SUBMODEL extends Model> void setSubContexts(Tag<?> element, Function<ModelContext, ObservableList<SUBMODEL>> applyOnModelContext, ViewContext<?> viewContext) {
		subContextsMap.put(element, new TransformationObservableList<SUBMODEL, ModelContext>(applyOnModelContext.apply(this), (index, model) -> createChildContext(model, viewContext, index, element), ModelContext::destroy));
	}

	public <SUBMODEL extends Model> ObservableList<SUBMODEL> getObservableSubModels(Tag<SUBMODEL> element) {
		return (ObservableList<SUBMODEL>) observableSubModels.get(element);
	}

	public <SUBMODEL extends Model> ObservableList<SUBMODEL> setObservableSubModels(Tag<SUBMODEL> element, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<GenericModel> constructor) {
		assert observableSubModels.get(element) == null;
		Generic[] gs = this.<GenericModel> getModel().getGenerics();
		ObservableList<SUBMODEL> result = new TransformationObservableList<Generic, SUBMODEL>(observableListExtractor.apply(gs), generic -> (SUBMODEL) constructor.build(GenericModel.addToGenerics(generic, gs), stringExtractor));
		observableSubModels.put(element, result);
		return result;
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

	public static class RootModelContext extends ModelContext {
		public RootModelContext(Model model) {
			super(null, model);
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
