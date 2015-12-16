package org.genericsystem.ui.components;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

import org.genericsystem.ui.Element;

public class GSPane<Component extends GSPane<Component, N>, N extends Pane> extends GSRegion<Component, N> {

	public GSPane(Element<?> parent, Class<N> paneClass) {
		super(parent, paneClass);
	}

	public <PARENTNODE> GSPane(Element<?> parent, Class<N> paneClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M, T> Component forEach(Function<M, ObservableList<T>> function, Function<T, Property<M>> injectedProperty, Consumer<Element<N>> subModelInit) {
		super.forEach(function, injectedProperty, subModelInit);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M, T> Component forEach(Function<M, ObservableList<T>> function, Consumer<Element<N>> subModelInit) {
		super.forEach(function, subModelInit);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M, T> Component select(Function<M, ObservableValue<T>> function) {
		super.select(function);
		return (Component) this;
	}

}
