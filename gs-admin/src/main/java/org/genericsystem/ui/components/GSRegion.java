package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.layout.Region;

import org.genericsystem.ui.Element;

public abstract class GSRegion<Component extends GSNode<Component, N>, N extends Region> extends GSNode<Component, N> {

	public GSRegion(Element parent, Class<N> class1) {
		super(parent, class1);
	}

	public <PARENTNODE> GSRegion(Element parent, Class<N> paneClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

	public Component setPrefWidth(Number prefWidth) {
		addBoot(N::prefWidthProperty, prefWidth);
		return (Component) this;
	}

	public <M> Component setOptionalPrefWidth(Function<M, ObservableValue<Number>> observablePrefWidth, Number prefWidth) {
		addBinding(N::prefWidthProperty, observablePrefWidth);
		return (Component) this;
	}

	public Component setPrefHeight(Number prefHeight) {
		addBoot(N::prefHeightProperty, prefHeight);
		return (Component) this;
	}

	public <M> Component setOptionalPrefHeight(Function<M, ObservableValue<Number>> observablePrefHeight, Number prefWidth) {
		addBinding(N::prefHeightProperty, observablePrefHeight);
		return (Component) this;
	}
}
