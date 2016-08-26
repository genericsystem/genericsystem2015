package org.genericsystem.reactor.gs;

import java.util.function.BiFunction;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.DisplayDefaults;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;

import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public abstract class GSTag extends Tag<GenericModel> implements GenericStringDefaults, DisplayDefaults {
	public GSTag(GSTag parent, String tag) {
		super(parent, tag);
	}

	public void forEach(ObservableListExtractor observableListExtractor) {
		super.forEach(model -> observableListExtractor.apply(((GenericModel) model).getGenerics()), MetaBinding.MODEL_BUILDER);
	}

	protected void forEach(GSTag parentCompositeElement) {
		forEach(gs -> parentCompositeElement.getObservableListExtractor().apply(gs));
	}

	public void select(Function<Generic[], Generic> genericSupplier) {
		forEach((ObservableListExtractor) gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	public void select_(Function<GenericModel, ObservableValue<GenericModel>> applyOnModelContext) {
		select__(model -> new ListBinding<GenericModel>() {
			ObservableValue<GenericModel> ov = applyOnModelContext.apply(model);
			{
				bind(ov);
			}

			@Override
			protected ObservableList<GenericModel> computeValue() {
				GenericModel model = ov.getValue();
				return model != null ? FXCollections.singletonObservableList(model) : FXCollections.emptyObservableList();
			}
		});
	}

	public void select__(Function<GenericModel, ObservableList<GenericModel>> applyOnModelContext) {
		super.forEach(model -> applyOnModelContext.apply((GenericModel) model), MetaBinding.MODEL_CLONER);
	}

	public void select(BiFunction<GenericModel, ObservableList<Generic>, ObservableList<GenericModel>> applyOnModel) {
		select__(model -> new ListBinding<GenericModel>() {
			ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
			{
				bind(holders);
			}

			@Override
			protected ObservableList<GenericModel> computeValue() {
				return applyOnModel.apply(model, holders);
			}
		});
	}

	public void select(Class<?> genericClass) {
		forEach((ObservableListExtractor) gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	public ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}
}
