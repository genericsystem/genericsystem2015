package org.genericsystem.reactor;

import java.util.function.Function;

import org.genericsystem.defaults.tools.TransformationObservableList;

import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 *
 * @param <M>
 */
public interface MetaBinding<M extends Model> {

	public void init(ViewContext<?> viewContext, Element<?> childElement);

	public static <SUBMODEL extends Model> MetaBinding<SUBMODEL> forEach(Function<ModelContext, ObservableList<SUBMODEL>> applyOnModelContext) {
		return (viewContext, childElement) -> viewContext.getModelContext().getSubContextsMap().put(childElement,
				new TransformationObservableList<SUBMODEL, ModelContext>(applyOnModelContext.apply(viewContext.getModelContext()),
						(index, model) -> viewContext.getModelContext().createChildContext(model, viewContext, index, childElement), ModelContext::destroy));
	}
}
