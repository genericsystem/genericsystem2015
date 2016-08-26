package org.genericsystem.reactor;

import java.util.function.BiFunction;
import java.util.function.Function;

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
}