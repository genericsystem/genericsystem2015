package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Node;

import org.genericsystem.ui.Element;

public abstract class GSNode<Component extends GSNode<Component, N>, N extends Node> extends Element<N> {

	public GSNode(Element parent, Class<N> nodeClass) {
		super(parent, nodeClass);
	}

	public <PARENTNODE> GSNode(Element parent, Class<N> paneClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

	public <M> Component setStyleClass(String text) {
		addObservableListBoot(N::getStyleClass, text);
		return (Component) this;
	}

	public <M> Component setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(N::getStyleClass, function);
		return (Component) this;
	}

	public <M> Component setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(N::getStyleClass, function, text);
		return (Component) this;
	}

	public <M> Component setVisibility(boolean visibility) {
		addBoot(N::visibleProperty, visibility);
		return (Component) this;
	}

	public <M> Component setOptionalVisibility(Function<M, ObservableValue<Boolean>> observableVisibility) {
		addBinding(N::visibleProperty, observableVisibility);
		return (Component) this;
	}

}
