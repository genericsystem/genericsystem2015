package org.genericsystem.ui.components;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.layout.Region;

import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.Model;

public abstract class GSRegion<Component extends GSNode<Component, N>, N extends Region> extends GSNode<Component, N> {

	public GSRegion(Element<?> parent, Class<N> clazz) {
		super(parent, clazz);
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Function<N, List> getGraphicChildren() {
		return parentNode -> Collections.emptyList();
	}

	@SuppressWarnings("unchecked")
	public Component setPrefWidth(Number prefWidth) {
		addBoot(N::prefWidthProperty, prefWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setPrefWidth(Function<M, ObservableValue<Number>> observablePrefWidth) {
		addBinding(N::prefWidthProperty, observablePrefWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setPrefHeight(Number prefHeight) {
		addBoot(N::prefHeightProperty, prefHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setPrefHeight(Function<M, ObservableValue<Number>> observablePrefHeight) {
		addBinding(N::prefHeightProperty, observablePrefHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setMinWidth(Number minWidth) {
		addBoot(N::minWidthProperty, minWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setMinWidth(Function<M, ObservableValue<Number>> observableMinWidth) {
		addBinding(N::minWidthProperty, observableMinWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setMinHeight(Number minHeight) {
		addBoot(N::minHeightProperty, minHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setMinHeight(Function<M, ObservableValue<Number>> observableMinHeight) {
		addBinding(N::minHeightProperty, observableMinHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setMaxWidth(Number maxWidth) {
		addBoot(N::maxWidthProperty, maxWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setMaxWidth(Function<M, ObservableValue<Number>> observableMaxWidth) {
		addBinding(N::maxWidthProperty, observableMaxWidth);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setMaxHeight(Number maxHeight) {
		addBoot(N::maxHeightProperty, maxHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public <M> Component setMaxHeight(Function<M, ObservableValue<Number>> observableMaxHeight) {
		addBinding(N::maxHeightProperty, observableMaxHeight);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M extends Model, T extends Model> Component forEach(Function<M, ObservableList<T>> function) {
		super.forEach(function);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M extends Model, T extends Model> Component select(Function<M, ObservableValue<T>> function) {
		super.select(function);
		return (Component) this;
	}

	@SuppressWarnings("unchecked")
	public Component setPadding(Insets inset) {
		addBoot(Region::paddingProperty, inset);
		return (Component) this;
	}

}
