package org.genericsystem.gsadmin;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.Listable;

public class Table extends Listable<Row> {

	private final Property<Number> rowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> columnWidth = new SimpleIntegerProperty(80);
	private final ObservableValue<Number> tableHeight = Bindings.multiply(getElements().size() + 1, (ObservableNumberValue) rowHeight);
	private final ObservableValue<Number> tableWidth = Bindings.multiply(getFirstElement().getValue().getElements().size() + 1, (ObservableNumberValue) columnWidth);

	private final Property<Number> scrollableTableHeight = new SimpleIntegerProperty(Integer.MAX_VALUE);
	private final Property<Number> scrollableTableWidth = new SimpleIntegerProperty(Integer.MAX_VALUE);

	public Table(ObservableValue<Row> firstRow, ObservableList<Row> rows, ObservableValue<String> tableStyle) {
		super(firstRow, rows, tableStyle);
	}

	public Property<Number> getRowHeight() {
		return rowHeight;
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

	public Property<Number> getScrollableTableWidth() {
		return scrollableTableWidth;
	}

	public Property<Number> getScrollableTableHeight() {
		return scrollableTableHeight;
	}
}
