package org.genericsystem.ui.table;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;

import org.genericsystem.distributed.ui.Model;

public abstract class Window extends Model {
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;

	private final ObservableValue<Number> scrollHeight;

	public Window(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;

		this.scrollHeight = Bindings.subtract((ObservableNumberValue) height, Integer.valueOf(25));
	}

	public ObservableValue<Number> getWidth() {
		return width;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public ObservableValue<Number> getScrollHeight() {
		return scrollHeight;
	}
}
