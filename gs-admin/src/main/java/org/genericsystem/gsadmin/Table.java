package org.genericsystem.gsadmin;

import java.util.concurrent.Callable;

import javafx.beans.binding.Binding;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.When;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableBooleanValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.Listable;

public class Table extends Listable<Row> {

	private final Property<Number> rowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> firstRowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> columnWidth = new SimpleIntegerProperty(80);
	private final Property<Number> firstColumnWidth = new SimpleIntegerProperty(80);
	private final ObservableValue<Number> tableWidth = Bindings.add(Bindings.multiply(getWidth(), (ObservableNumberValue) columnWidth),getWidthFirstRow());
	private final ObservableValue<Number> tableHeight = Bindings.add(Bindings.multiply(getElements().size(), (ObservableNumberValue) rowHeight), getHeightFirstRow());

	private ObservableNumberValue getWidth(){
		ObservableBooleanValue isFirstElementNull =Bindings.createBooleanBinding(()-> getFirstElement().getValue() == null, new SimpleObjectProperty<>(getFirstElement().getValue()) );
		return new SimpleIntegerProperty((int) Bindings.when(isFirstElementNull).then(getElements().size()).otherwise(!isFirstElementNull.getValue() ? getFirstElement().getValue().getElements().size():getElements().size()-1).getValue().intValue());
	}
	
	private ObservableNumberValue  getWidthFirstRow(){
		return getFirstElement().getValue()!=null?(ObservableNumberValue) firstColumnWidth:new SimpleIntegerProperty(1);
	}
	
	private ObservableNumberValue  getHeightFirstRow(){
		return getFirstElement().getValue()!=null?(ObservableNumberValue) firstRowHeight:new SimpleIntegerProperty(1);
	}
	
	public Table(ObservableValue<Row> firstRow, ObservableList<Row> rows, ObservableValue<String> tableStyle) {
		super(firstRow, rows, tableStyle);
	}

	public Property<Number> getFirstRowHeight() {
		return firstRowHeight;
	}

	public Property<Number> getRowHeight() {
		return rowHeight;
	}

	public Property<Number> getFirstColumnWidth() {
		return firstColumnWidth;
	}

	public Property<Number> getColumnWidth() {
		return columnWidth;
	}

	public ObservableValue<Number> getTableHeight() {
		return tableHeight;
	}

	public ObservableValue<Number> getTableWidth() {
		return tableWidth;
	}
}
