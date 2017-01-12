package org.genericsystem.reactor;

import java.util.function.BiFunction;
import java.util.function.Function;

import org.genericsystem.common.Generic;

import javafx.collections.ObservableList;

public class MetaBinding<BETWEEN> {
	private Function<Context, ObservableList<BETWEEN>> betweenChildren;
	private final BiFunction<Context, BETWEEN, Context> modelBuilder;

	private static BiFunction<Context, Generic, Context> MODEL_BUILDER = (model, generic) -> new Context(model, Context.addToGenerics(generic, model.getGenerics()));
	private static BiFunction<Context, Context, Context> MODEL_CLONER = (model, subModel) -> new Context(model, subModel.getGenerics());

	public MetaBinding(Function<Context, ObservableList<BETWEEN>> betweenChildren, BiFunction<Context, BETWEEN, Context> modelBuilder) {
		this.betweenChildren = betweenChildren;
		this.modelBuilder = modelBuilder;
	}

	public ObservableList<BETWEEN> buildBetweenChildren(Context context) {
		return betweenChildren.apply(context);
	}

	public Function<Context, ObservableList<BETWEEN>> getBetweenChildren() {
		return betweenChildren;
	}

	public Context buildModel(Context parent, BETWEEN betweenChild) {
		return modelBuilder.apply(parent, betweenChild);
	}

	public static MetaBinding<Context> selectMetaBinding(Function<Context, ObservableList<Context>> betweenChildren) {
		return new MetaBinding<Context>(betweenChildren, MODEL_CLONER);
	}

	public static MetaBinding<Generic> forEachMetaBinding(Function<Context, ObservableList<Generic>> betweenChildren) {
		return new MetaBinding<Generic>(betweenChildren, MODEL_BUILDER);
	}

	public void setBetweenChildren(Function<Context, ObservableList<BETWEEN>> betweenChildren) {
		this.betweenChildren = betweenChildren;
	}

}