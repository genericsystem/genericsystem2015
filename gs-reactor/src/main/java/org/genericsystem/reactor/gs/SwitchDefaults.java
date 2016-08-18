package org.genericsystem.reactor.gs;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyIntegerWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public interface SwitchDefaults {

	public static final String SUBMODELS = "subModels";
	public static final String INDEX = "index";
	public static final String CURRENT_MODEL = "currentModel";

	void addPrefixBinding(Consumer<GenericModel> consumer);

	<T> void storePropertyWithoutCheck(String propertyName, GenericModel model, Function<GenericModel, ObservableValue<T>> applyOnModel);

	<T> Property<T> getProperty(String property, Model model);

	default ObservableList<GenericModel> getSwitchModels(GenericModel model) {
		Property<ObservableList<GenericModel>> modelsProperty = getProperty(SUBMODELS, model);
		return modelsProperty != null ? modelsProperty.getValue() : null;
	}

	default Property<GenericModel> getCurrentModel(GenericModel model) {
		return getProperty(CURRENT_MODEL, model);
	}

	default Property<Integer> getIteratorIndexProperty(GenericModel model) {
		return getProperty(INDEX, model);
	}

	default void switcher_(GSTag switchedTag, ObservableListExtractor observableListExtractor) {
		switcher(switchedTag, model -> FXCollections.observableArrayList(observableListExtractor.apply(model.getGenerics()).stream().map(g -> new GenericModel(model, GenericModel.addToGenerics(g, model.getGenerics()))).collect(Collectors.toList())));
	}

	default void switcher(GSTag switchedTag, Function<GenericModel, ObservableList<GenericModel>> applyOnModel) {
		addPrefixBinding(model -> {
			storePropertyWithoutCheck(SUBMODELS, model, m -> new SimpleObjectProperty<>(applyOnModel.apply(model)));
			storePropertyWithoutCheck(CURRENT_MODEL, model, m -> new SimpleObjectProperty<>(getSwitchModels(model).get(0)));
			storePropertyWithoutCheck(INDEX, model, m -> new ReadOnlyIntegerWrapper(0));
		});
		switchedTag.select_(model -> getCurrentModel(model));
	}

	default void next(GenericModel model) {
		Property<Integer> index = getIteratorIndexProperty(model);
		ObservableList<GenericModel> models = getSwitchModels(model);
		if (index.getValue() + 1 < models.size()) {
			index.setValue(index.getValue() + 1);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		}
	}

	default void prev(GenericModel model) {
		Property<Integer> index = getIteratorIndexProperty(model);
		ObservableList<GenericModel> models = getSwitchModels(model);
		if (index.getValue() > 0) {
			index.setValue(index.getValue() - 1);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		}
	}
}
