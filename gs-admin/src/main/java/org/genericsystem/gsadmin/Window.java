package org.genericsystem.gsadmin;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public class Window {
	
	private Property<Table> table = new SimpleObjectProperty<>();
	
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;
	
	private final CocClientEngine engine;
	
	public Window(CocClientEngine engine,Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.table = table;
		this.engine = engine;
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
	}

	public ObservableValue<Number> getWidth() {
		return width;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public ObservableValue<Table> getTable() {
		return table;
	}
	
	public void flush(){
			engine.getCurrentCache().flush();
	}
	
	public void cancel(){
		engine.getCurrentCache().clear();
	}
	
	public void mount(){
		engine.getCurrentCache().mount();
	}
	
	public void unmount(){
		engine.getCurrentCache().unmount();
	}
	
}
