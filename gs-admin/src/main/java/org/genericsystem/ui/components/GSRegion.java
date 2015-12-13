package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.Region;

import org.genericsystem.ui.Element;

public abstract class GSRegion<Component extends GSNode<Component, N>, N extends Region> extends GSNode<Component, N> {

	public GSRegion(Element parent, Class<N> regionClass) {
		super(parent, regionClass);
	}

	public Component setPrefWidth(Number prefWidth) {
		addBoot(N::prefWidthProperty, prefWidth);
		return (Component) this;
	}

	public <M> Component setOptionalPrefWidth(Function<M, ObservableValue<Number>> observablePrefWidth, Number prefWidth) {
		addBinding(N::prefWidthProperty, observablePrefWidth);
		return (Component) this;
	}
}
