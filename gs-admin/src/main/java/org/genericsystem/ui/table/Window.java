package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;

import org.genericsystem.ui.Model;

public abstract class Window extends Model {
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;

	public Window(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
	}

	public ObservableValue<Number> getWidth() {
		return width;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}
}
