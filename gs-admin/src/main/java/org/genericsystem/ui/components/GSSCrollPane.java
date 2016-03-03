package org.genericsystem.ui.components;

import java.util.List;
import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.ScrollPane;

import org.genericsystem.distributed.ui.Element;

public class GSSCrollPane extends Element<ScrollPane> {

	public GSSCrollPane(Element<?> parent) {
		super(parent, ScrollPane.class);
	}

	public <PARENTNODE> GSSCrollPane(Element<PARENTNODE> parent, Function<PARENTNODE, List<?>> getGraphicChildren) {
		super(parent, ScrollPane.class, getGraphicChildren);
	}

	public GSSCrollPane setPrefWidth(Number prefWidth) {
		addBoot(ScrollPane::prefWidthProperty, prefWidth);
		return this;
	}

	public <M> GSSCrollPane setPrefWidth(Function<M, ObservableValue<Number>> observablePrefWidth) {
		addBinding(ScrollPane::prefWidthProperty, observablePrefWidth);
		return this;
	}

	public GSSCrollPane setPrefHeight(Number prefHeight) {
		addBoot(ScrollPane::prefHeightProperty, prefHeight);
		return this;
	}

	public <M> GSSCrollPane setPrefHeight(Function<M, ObservableValue<Number>> observablePrefHeight) {
		addBinding(ScrollPane::prefHeightProperty, observablePrefHeight);
		return this;
	}

	public GSSCrollPane setMinWidth(Number minWidth) {
		addBoot(ScrollPane::minWidthProperty, minWidth);
		return this;
	}

	public <M> GSSCrollPane setMinWidth(Function<M, ObservableValue<Number>> observableMinWidth) {
		addBinding(ScrollPane::minWidthProperty, observableMinWidth);
		return this;
	}

	public GSSCrollPane setMinHeight(Number minHeight) {
		addBoot(ScrollPane::minHeightProperty, minHeight);
		return this;
	}

	public <M> GSSCrollPane setMinHeight(Function<M, ObservableValue<Number>> observableMinHeight) {
		addBinding(ScrollPane::minHeightProperty, observableMinHeight);
		return this;
	}

	public GSSCrollPane setMaxWidth(Number maxWidth) {
		addBoot(ScrollPane::maxWidthProperty, maxWidth);
		return this;
	}

	public <M> GSSCrollPane setMaxWidth(Function<M, ObservableValue<Number>> observableMaxWidth) {
		addBinding(ScrollPane::maxWidthProperty, observableMaxWidth);
		return this;
	}

	public GSSCrollPane setMaxHeight(Number maxHeight) {
		addBoot(ScrollPane::maxHeightProperty, maxHeight);
		return this;
	}

	public <M> GSSCrollPane setMaxHeight(Function<M, ObservableValue<Number>> observableMaxHeight) {
		addBinding(ScrollPane::maxHeightProperty, observableMaxHeight);
		return this;
	}

	public <M> GSSCrollPane setStyleClass(String text) {
		addObservableListBoot(ScrollPane::getStyleClass, text);
		return this;
	}

	public <M> GSSCrollPane setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(ScrollPane::getStyleClass, function);
		return this;
	}

	public <M> GSSCrollPane setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(ScrollPane::getStyleClass, function, text);
		return this;
	}

	public <M> GSSCrollPane setVisibility(boolean visibility) {
		addBoot(ScrollPane::visibleProperty, visibility);
		return this;
	}

	public <M> GSSCrollPane setVisibility(Function<M, ObservableValue<Boolean>> observableVisibility) {
		addBinding(ScrollPane::visibleProperty, observableVisibility);
		return this;
	}

	public <M> GSSCrollPane setMinViewPortHeight(Double height) {
		addBoot(ScrollPane::minViewportHeightProperty, height);
		return this;
	}

	public <M> GSSCrollPane setPrefViewPortHeight(Double height) {
		addBoot(ScrollPane::prefViewportHeightProperty, height);
		return this;
	}

	public <M> GSSCrollPane setPrefViewPortWidth(Double width) {
		addBoot(ScrollPane::prefViewportWidthProperty, width);
		return this;
	}

	public <M> GSSCrollPane setPrefViewPortHeight(Function<M, ObservableValue<Number>> height) {
		addBinding(ScrollPane::prefViewportHeightProperty, height);
		return this;
	}

	public <M> GSSCrollPane setPrefViewPortWidth(Function<M, ObservableValue<Number>> width) {
		addBinding(ScrollPane::prefViewportWidthProperty, width);
		return this;
	}

}
