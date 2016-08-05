package org.genericsystem.reactor.gs;

import java.util.List;
import java.util.function.Function;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public abstract class GSTag extends Tag<GenericModel> {
	public GSTag(GSTag parent, String tag) {
		super(parent, tag);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		setMetaBinding((childElement, viewContext) -> {
			GenericModel model = (GenericModel) viewContext.getModelContext();
			ObservableList<Generic> generics = observableListExtractor.apply(model.getGenerics());
			setSubModels(model, childElement, new TransformationObservableList<Generic, GenericModel>(generics, (index, generic) -> {
				// System.out.println("Change detected on : " + System.identityHashCode(generics) + " newValue : " + generic.info());
					GenericModel duplicate = new GenericModel(model, GenericModel.addToGenerics(generic, model.getGenerics()), stringExtractor);
					viewContext.createViewContextChild(index, duplicate, childElement);
					return duplicate;
				}, m -> {
					assert !model.destroyed;
					assert !m.destroyed;
					// TODO unregister viewContext before removing in list ?
					m.destroy();
				}));
		});
	}

	public void select_(Function<GenericModel, ObservableValue<GenericModel>> applyOnModel) {
		select_(null, applyOnModel);
	}

	public void select_(StringExtractor stringExtractor, Function<GenericModel, ObservableValue<GenericModel>> applyOnModelContext) {
		setMetaBinding((childElement, viewContext) -> {
			GenericModel model = (GenericModel) viewContext.getModelContext();
			ObservableValue<GenericModel> observableValue = applyOnModelContext.apply(model);
			ObservableList<GenericModel> subModels = FXCollections.observableArrayList();
			ChangeListener<GenericModel> listener = (ChangeListener<GenericModel>) (observable, oldValue, newValue) -> {
				if (oldValue != null)
					subModels.remove(0);
				if (newValue != null)
					subModels.add(newValue);
			};
			observableValue.addListener(listener);
			listener.changed(observableValue, null, observableValue.getValue());
			setSubModels(model, childElement, new TransformationObservableList<GenericModel, GenericModel>(subModels, (index, selectedModel) -> {
				Generic[] gs = selectedModel.getGenerics();
				// assert Arrays.equals(gs, gs2) : Arrays.toString(gs) + " vs " + Arrays.toString(gs2);
					GenericModel childModel = new GenericModel(model, gs, stringExtractor != null ? stringExtractor : selectedModel.getStringExtractor());
					viewContext.createViewContextChild(index, childModel, childElement);
					return childModel;
				}, Model::destroy));
		});
	}

	protected void forEach(GSTag parentCompositeElement) {
		forEach(g -> parentCompositeElement.getStringExtractor().apply(g), gs -> parentCompositeElement.getObservableListExtractor().apply(gs));
	}

	public void forEach(ObservableListExtractor observableListExtractor) {
		forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier) {
		forEach(stringExtractor, gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass) {
		forEach(stringExtractor, gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	public void select(Function<Generic[], Generic> genericSupplier) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier);
	}

	public void select(Class<?> genericClass) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericClass);
	}

	public StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	public ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	public int pos(Generic genericToUpdate, Generic oldComponent) {
		List<Generic> components = genericToUpdate.getComponents();
		int pos = 0;
		for (Generic component : components) {
			if (component.equals(oldComponent))
				break;
			pos++;
		}
		return pos;
	}
}
