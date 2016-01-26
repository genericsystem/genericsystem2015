package org.genericsystem.ui.table;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.table.Stylable.Listable;

public class Table extends Listable<Row> {

	private final Property<Number> rowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> firstRowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> firstColumnWidth = new SimpleIntegerProperty(100);
	private final Property<Number> columnWidth = new SimpleIntegerProperty(100);
	private final Property<Number> lastColumnWidth = new SimpleIntegerProperty(100);
	// private final Property<Number> secondColumnWidth = new SimpleIntegerProperty(300);

	private final ObservableValue<Row> referenceRow = Bindings.createObjectBinding(() -> getReferenceRow().getValue(), getFirstElement(), getElements());
	private final ObservableIntegerValue firstRowNumber = Bindings.createIntegerBinding(() -> getFirstElement().getValue() != null ? 1 : 0, getFirstElement());
	private final ObservableNumberValue tableHeight = Bindings.createIntegerBinding(() -> Bindings.add(getOptionalFirstRowHeight(), getOtherRowsHeight()).intValue(), getElements());
	private final ObservableIntegerValue firstCellNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getFirstElement().getValue() != null ? 1 : 0 : 0, referenceRow);
	private final ObservableIntegerValue otherCellsNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getElements().size() : 0, referenceRow);
	private final ObservableIntegerValue lastCellNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getLastElement().getValue() != null ? 1 : 0 : 0, referenceRow);
	private final ObservableValue<Number> tableWidth = Bindings.add(getOptionalFirstCellWidth(), Bindings.add(getOtherCellsWidth(), getOptionalLastCellWidth()));

	private Property<Row> selectedRow = new SimpleObjectProperty<>();

	private ObservableValue<Row> getReferenceRow() {
		if (getFirstElement().getValue() != null) {
			return getFirstElement();
		} else if (getElements().size() != 0) {
			return new SimpleObjectProperty<>(getElements().get(0));
		} else {
			return new SimpleObjectProperty<>();
		}
	}

	public Property<Row> getSelectedRow() {
		return selectedRow;
	}

	private ObservableNumberValue getOptionalFirstRowHeight() {
		return Bindings.multiply(firstRowNumber, (ObservableNumberValue) firstRowHeight);
	}

	private ObservableNumberValue getOtherRowsHeight() {
		return Bindings.multiply(getElements().size(), (ObservableNumberValue) rowHeight);
	}

	private ObservableNumberValue getOptionalFirstCellWidth() {
		return Bindings.multiply(firstCellNumber, (ObservableNumberValue) firstColumnWidth);
	}

	private ObservableNumberValue getOtherCellsWidth() {
		return Bindings.multiply(otherCellsNumber, (ObservableNumberValue) columnWidth);
	}

	private ObservableNumberValue getOptionalLastCellWidth() {
		return Bindings.multiply(lastCellNumber, (ObservableNumberValue) lastColumnWidth);
	}

	public Table(ObservableValue<Row> firstRow, ObservableList<Row> rows, ObservableValue<String> tableStyle) {
		super(firstRow, rows, null, tableStyle);
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

	public Property<Number> getLastColumnWidth() {
		return lastColumnWidth;
	}

	public ObservableNumberValue getTableHeight() {
		return tableHeight;
	}

	public ObservableValue<Number> getTableWidth() {
		return tableWidth;
	}
}
