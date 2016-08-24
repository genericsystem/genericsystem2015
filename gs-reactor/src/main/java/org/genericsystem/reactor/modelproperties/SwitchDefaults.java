package org.genericsystem.reactor.modelproperties;

import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyIntegerWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public interface SwitchDefaults extends ModelProperty<GenericModel> {

	public static final String SUBMODELS = "subModels";
	public static final String INDEX = "index";
	public static final String CURRENT_MODEL = "currentModel";
	public static final String NAME_MODEL = "nameModel";

	default ObservableList<GenericModel> getSwitchModels(GenericModel model) {
		Property<ObservableList<GenericModel>> modelsProperty = getProperty(SUBMODELS, model);
		return modelsProperty != null ? modelsProperty.getValue() : null;
	}

	default Property<GenericModel> getCurrentModel(GenericModel model) {
		return getProperty(CURRENT_MODEL, model);
	}

	default Property<GenericModel> getNameModel(GenericModel model) {
		return getProperty(NAME_MODEL, model);
	}

	default Property<Integer> getIteratorIndexProperty(GenericModel model) {
		return getProperty(INDEX, model);
	}

	default void switcher_(GSTag switchedTag, ObservableListExtractor observableListExtractor, GSTag instanceNameTag) {
		switcher(switchedTag, model -> FXCollections.observableArrayList(observableListExtractor.apply(model.getGenerics()).stream().map(g -> new GenericModel(model, GenericModel.addToGenerics(g, model.getGenerics()))).collect(Collectors.toList())),
				instanceNameTag);
	}

	default void switcher(GSTag switchedTag, Function<GenericModel, ObservableList<GenericModel>> applyOnModel, GSTag instanceNameTag) {
		addPrefixBinding(model -> {
			storePropertyWithoutCheck(SUBMODELS, model, m -> new SimpleObjectProperty<>(applyOnModel.apply(model)));
			storePropertyWithoutCheck(CURRENT_MODEL, model, m -> new SimpleObjectProperty<>());
			storePropertyWithoutCheck(NAME_MODEL, model, m -> new SimpleObjectProperty<>(model));
			storePropertyWithoutCheck(INDEX, model, m -> new ReadOnlyIntegerWrapper(-1));
		});
		instanceNameTag.select_(model -> getNameModel(model));
		switchedTag.select_(model -> getCurrentModel(model));
	}

	default void next(GenericModel model) {
		Property<Integer> index = getIteratorIndexProperty(model);
		ObservableList<GenericModel> models = getSwitchModels(model);
		if (index.getValue() == -1) {
			index.setValue(0);
			getNameModel(model).setValue(null);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		} else if (index.getValue() + 1 < models.size()) {
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
		} else if (index.getValue() == 0) {
			index.setValue(-1);
			getNameModel(model).setValue((GenericModel) model.getParent());
			getCurrentModel(model).setValue(null);
		}
	}
}
