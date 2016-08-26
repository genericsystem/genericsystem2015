package org.genericsystem.reactor;

import java.util.function.BiFunction;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.GenericModel;

import javafx.collections.ObservableList;

public class MetaBinding<BETWEEN> {
	private final Function<Model, ObservableList<BETWEEN>> betweenChildren;
	private final BiFunction<Model, BETWEEN, Model> modelBuilder;

	public MetaBinding(Function<Model, ObservableList<BETWEEN>> betweenChildren, BiFunction<Model, BETWEEN, Model> modelBuilder) {
		this.betweenChildren = betweenChildren;
		this.modelBuilder = modelBuilder;
	}

	public ObservableList<BETWEEN> buildBetweenChildren(Model model) {
		return betweenChildren.apply(model);
	}

	public Model buildModel(Model parent, BETWEEN betweenChild) {
		return modelBuilder.apply(parent, betweenChild);
	}

	public static BiFunction<Model, Generic, Model> MODEL_BUILDER = (model, generic) -> new GenericModel(model, GenericModel.addToGenerics(generic, ((GenericModel) model).getGenerics()));

	public static BiFunction<Model, GenericModel, Model> MODEL_CLONER = (model, subModel) -> new GenericModel(model, subModel.getGenerics());
}