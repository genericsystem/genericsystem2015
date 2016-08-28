package org.genericsystem.reactor;

import java.util.function.BiFunction;
import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;

public class MetaBinding<BETWEEN> {
	private final Function<Context, ObservableList<BETWEEN>> betweenChildren;
	private final BiFunction<Context, BETWEEN, Context> modelBuilder;

	public MetaBinding(Function<Context, ObservableList<BETWEEN>> betweenChildren, BiFunction<Context, BETWEEN, Context> modelBuilder) {
		this.betweenChildren = betweenChildren;
		this.modelBuilder = modelBuilder;
	}

	public ObservableList<BETWEEN> buildBetweenChildren(Context model) {
		return betweenChildren.apply(model);
	}

	public Context buildModel(Context parent, BETWEEN betweenChild) {
		return modelBuilder.apply(parent, betweenChild);
	}

	public static BiFunction<Context, Generic, Context> MODEL_BUILDER = (model, generic) -> new Context(model, Context.addToGenerics(generic, model.getGenerics()));

	public static BiFunction<Context, Context, Context> MODEL_CLONER = (model, subModel) -> new Context(model, subModel.getGenerics());
}