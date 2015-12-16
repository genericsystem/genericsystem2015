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
	public <M, T> Component forEach(Function<M, ObservableList<T>> function, Function<T, Property<M>> injectedProperty) {
		super.forEach(function, injectedProperty);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M, T> Component forEach(Function<M, ObservableList<T>> function) {
		super.forEach(function);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M, T> Component select(Function<M, ObservableValue<T>> function) {
		super.select(function);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component include(Consumer<Element<N>> subModelInit) {
		subModelInit.accept(this);
		return (Component) this;
	}

}
