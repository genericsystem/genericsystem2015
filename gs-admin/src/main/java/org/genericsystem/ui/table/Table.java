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

	private final ObservableValue<Row> referenceRow = Bindings.createObjectBinding(() -> getReferenceRow().getValue(), getFirstElement(), getElements());
	private final ObservableIntegerValue firstRowNumber = Bindings.createIntegerBinding(() -> getFirstElement().getValue() != null ? 1 : 0, getFirstElement());
	private ObservableNumberValue tableHeight = Bindings.createIntegerBinding(() -> Bindings.add(getOptionalFirstRowHeight(), getOtherRowsHeight()).intValue(), getElements());
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
		if (getParent() instanceof Cell)
			return Bindings.multiply(firstCellNumber, (ObservableNumberValue) ((Table) getParent().getParent()).firstColumnWidth);
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
		if (getParent() instanceof Cell)
			rowHeight.setValue(((Table) getParent().getParent()).rowHeight.getValue());
		return rowHeight;
	}

	public Property<Number> getFirstColumnWidth() {
		if (getParent() instanceof Cell)
			firstColumnWidth.setValue(((Table) getParent().getParent()).firstColumnWidth.getValue());
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

	public ObservableValue<Number> getParentWidth() {
		if (getParent() instanceof Cell) {
			return new SimpleObjectProperty<Number>(((Table) getParent().getParent()).columnWidth.getValue());
		}
		return new SimpleObjectProperty<Number>(((Window) getParent().getParent()).getWidth().getValue());
	}

	public ObservableValue<Number> getParentHeight() {
		if (getParent() instanceof Cell) {

			return new SimpleObjectProperty<Number>(((Table) getParent().getParent().getParent()).rowHeight.getValue());
		}
		System.out.println(((Window) getParent().getParent()).getHeight().getValue());
		return new SimpleObjectProperty<Number>(((Window) getParent().getParent()).getHeight().getValue());
	}
}
