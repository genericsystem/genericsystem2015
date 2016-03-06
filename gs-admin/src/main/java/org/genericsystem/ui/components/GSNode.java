package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.scene.Node;

import org.genericsystem.distributed.ui.Element;

public abstract class GSNode<Component extends GSNode<Component, N>, N extends Node> extends Element<N> {

	public <PARENTNODE> GSNode(Element<PARENTNODE> parent, Class<N> nodeClass) {
		super(parent, nodeClass);
	}

	@SuppressWarnings("unchecked")
	public <M> Component setStyleClass(String text) {
		addObservableListBoot(N::getStyleClass, text);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(N::getStyleClass, function);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(N::getStyleClass, function, text);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setVisibility(boolean visibility) {
		addBoot(N::visibleProperty, visibility);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setOptionalVisibility(Function<M, ObservableValue<Boolean>> observableVisibility) {
		addBinding(N::visibleProperty, observableVisibility);
		return (Component) this;
	}

}
