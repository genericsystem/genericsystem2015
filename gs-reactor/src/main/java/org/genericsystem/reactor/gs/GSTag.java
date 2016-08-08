package org.genericsystem.reactor.gs;

import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public abstract class GSTag extends Tag<GenericModel> {
	public GSTag(GSTag parent, String tag) {
		super(parent, tag);
	}

	public void forEachGeneric(ObservableListExtractor observableListExtractor) {
		super.forEach(model -> observableListExtractor.apply(((GenericModel) model).getGenerics()), (model, subElement) -> new GenericModel(model, GenericModel.addToGenerics(subElement, ((GenericModel) model).getGenerics())));
	}

	protected void forEachGeneric(GSTag parentCompositeElement) {
		forEachGeneric(gs -> parentCompositeElement.getObservableListExtractor().apply(gs));
	}

	// TODO
	// 1) selection doesn't listen really
	// 2) is the generic accumulation in context a good idea here ?
	public void select(Function<Generic[], Generic> genericSupplier) {
		forEachGeneric((ObservableListExtractor) gs -> {
			Generic generic = genericSupplier.apply(gs);
			return generic != null ? FXCollections.singletonObservableList(generic) : FXCollections.emptyObservableList();
		});
	}

	public void select_(Function<GenericModel, ObservableValue<GenericModel>> applyOnModelContext) {
		super.forEach(model -> new ListBinding<GenericModel>() {
			ObservableValue<GenericModel> ov = applyOnModelContext.apply((GenericModel) model);
			{
				bind(ov);
			}

			@Override
			protected ObservableList<GenericModel> computeValue() {
				GenericModel model = ov.getValue();
				return model != null ? FXCollections.singletonObservableList(model) : FXCollections.emptyObservableList();
			}
		}, (model, subElement) -> new GenericModel(model, subElement.getGenerics()));
	}

	public void select(Class<?> genericClass) {
		forEachGeneric((ObservableListExtractor) gs -> FXCollections.singletonObservableList(gs[0].getRoot().find(genericClass)));
	}

	public ObservableValue<String> getString(GenericModel model) {
		return model.getString(getStringExtractor(model));
	}

	public void bindGenericText() {
		addPrefixBinding(model -> model.getTextProperty(this).bind(getString(model)));
	}

	public StringExtractor getStringExtractor(GenericModel model) {
		Property<StringExtractor> stringExtractorProperty = getProperty(ReactorStatics.EXTRACTOR, model);
		return stringExtractorProperty != null ? stringExtractorProperty.getValue() : StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	public void setStringExtractor(StringExtractor extractor) {
		addPrefixBinding(modelContext -> {
			if (!modelContext.containsProperty(this, ReactorStatics.EXTRACTOR))
				modelContext.createNewProperty(this, ReactorStatics.EXTRACTOR);
			getProperty(ReactorStatics.EXTRACTOR, modelContext).setValue(extractor);
		});
	}

	public ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}
}
