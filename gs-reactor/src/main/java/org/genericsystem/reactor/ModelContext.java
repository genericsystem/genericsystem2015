package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Element.SelectableActionHtmlNode;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.model.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;

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
	private final Map<Element<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();
	private final Map<Element<?>, List<ModelContext>> subContextsMap = new HashMap<>();
	private final Map<Element<?>, ObservableList<?>> observableModels = new HashMap<>();

	private ModelContext(ModelContext parent, Model model) {
		this.parent = parent;
		this.model = model;
	}

	public ModelContext createChildContext(Model childModel, ViewContext<?> viewContext, int index, Element<?> childElement) {
		childModel.parent = getModel();// inject parent
		childModel.afterParentConstruct();
		ModelContext modelContextChild = new ModelContext(this, childModel);
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

	public Map<Element<?>, List<ModelContext>> getSubContextsMap() {
		return subContextsMap;
	}

	public <M extends Model> ObservableList<M> getObservableSubModels(Element<?> element, StringExtractor stringExtractor,
			ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		ObservableList<M> result = (ObservableList<M>) observableModels.get(element);
		if (result == null) {
			Generic[] gs = this.<CompositeModel> getModel().getGenerics();
			result = new TransformationObservableList<Generic, M>(observableListExtractor.apply(gs),
					generic -> (M) constructor.build(CompositeModel.addToGenerics(generic, gs), stringExtractor));
			observableModels.put(element, result);
		}
		return result;
	}

	public void register(ViewContext<?> viewContext) {
		ViewContext<?> previous = viewContextsMap.put(viewContext.getElement(), viewContext);
		assert previous == null;
	}

	public void destroy() {
		for (ViewContext<?> viewContext : viewContextsMap.values())
			viewContext.destroyChild();
	}

	public static class RootModelContext extends ModelContext {
		public RootModelContext(Model model) {
			super(null, model);
		}
	}

	public ViewContext<?> getViewContext(Element<?> element) {
		return viewContextsMap.get(element);
	}

	public Property<String> getTextProperty(Element<?> element) {
		return getViewContext(element).getNode().getTextProperty();
	}

	public ObservableSet<String> getObservableStyleClasses(Element<?> element) {
		return getViewContext(element).getNode().getStyleClasses();
	}

	public ObservableMap<String, String> getObservableStyles(Element<?> element) {
		return getViewContext(element).getNode().getStyles();
	}

	public Property<Number> getSelectionIndex(Element<?> element) {
		return ((SelectableActionHtmlNode) getViewContext(element).getNode()).getSelectionIndex();
	}
}
