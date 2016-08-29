package org.genericsystem.reactor.modelproperties;

import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyIntegerWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.ObservableListExtractor;

public interface SwitchDefaults extends ModelProperty {

	public static final String SUBMODELS = "subModels";
	public static final String INDEX = "index";
	public static final String CURRENT_MODEL = "currentModel";
	public static final String NAME_MODEL = "nameModel";

	default ObservableList<Context> getSwitchModels(Context model) {
		Property<ObservableList<Context>> modelsProperty = getProperty(SUBMODELS, model);
		return modelsProperty != null ? modelsProperty.getValue() : null;
	}

	default Property<Context> getCurrentModel(Context model) {
		return getProperty(CURRENT_MODEL, model);
	}

	default Property<Context> getNameModel(Context model) {
		return getProperty(NAME_MODEL, model);
	}

	default Property<Integer> getIteratorIndexProperty(Context model) {
		return getProperty(INDEX, model);
	}

	default void switcher_(Tag switchedTag, ObservableListExtractor observableListExtractor, Tag instanceNameTag) {
		switcher(switchedTag, model -> FXCollections.observableArrayList(observableListExtractor.apply(model.getGenerics()).stream().map(g -> new Context(model, Context.addToGenerics(g, model.getGenerics()))).collect(Collectors.toList())), instanceNameTag);
	}

	default void switcher(Tag switchedTag, Function<Context, ObservableList<Context>> applyOnModel, Tag instanceNameTag) {
		addPrefixBinding(model -> {
			storeProperty(SUBMODELS, model, m -> new SimpleObjectProperty<>(applyOnModel.apply(model)));
			storeProperty(CURRENT_MODEL, model, m -> new SimpleObjectProperty<>());
			storeProperty(NAME_MODEL, model, m -> new SimpleObjectProperty<>(model));
			storeProperty(INDEX, model, m -> new ReadOnlyIntegerWrapper(-1));
		});
		instanceNameTag.select__(model -> getNameModel(model));
		switchedTag.select__(model -> getCurrentModel(model));
	}

	default void next(Context model) {
		Property<Integer> index = getIteratorIndexProperty(model);
		ObservableList<Context> models = getSwitchModels(model);
		if (index.getValue() == -1) {
			index.setValue(0);
			getNameModel(model).setValue(null);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		} else if (index.getValue() + 1 < models.size()) {
			index.setValue(index.getValue() + 1);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		}
	}

	default void prev(Context model) {
		Property<Integer> index = getIteratorIndexProperty(model);
		ObservableList<Context> models = getSwitchModels(model);
		if (index.getValue() > 0) {
			index.setValue(index.getValue() - 1);
			getCurrentModel(model).setValue(models.get(index.getValue()));
		} else if (index.getValue() == 0) {
			index.setValue(-1);
			getNameModel(model).setValue(model.getParent());
			getCurrentModel(model).setValue(null);
		}
	}
}
