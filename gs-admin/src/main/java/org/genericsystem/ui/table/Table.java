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

	protected final Property<Number> rowHeight = new SimpleIntegerProperty(20);
	protected final Property<Number> firstRowHeight = new SimpleIntegerProperty(20);
	protected final Property<Number> firstColumnWidth = new SimpleIntegerProperty(100);
	protected final Property<Number> columnWidth = new SimpleIntegerProperty(100);
	protected final Property<Number> lastColumnWidth = new SimpleIntegerProperty(100);

	protected final ObservableValue<Row> referenceRow = Bindings.createObjectBinding(() -> getReferenceRow().getValue(), getFirstElement(), getElements());
	protected final ObservableIntegerValue firstRowNumber = Bindings.createIntegerBinding(() -> getFirstElement().getValue() != null ? 1 : 0, getFirstElement());
	protected ObservableNumberValue tableHeight = Bindings.createIntegerBinding(() -> Bindings.add(getOptionalFirstRowHeight(), getOtherRowsHeight()).intValue(), getElements());
	protected final ObservableIntegerValue firstCellNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getFirstElement().getValue() != null ? 1 : 0 : 0, referenceRow);
	protected final ObservableIntegerValue otherCellsNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getElements().size() : 0, referenceRow);
	protected final ObservableIntegerValue lastCellNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getLastElement().getValue() != null ? 1 : 0 : 0, referenceRow);
	protected final ObservableValue<Number> tableWidth = Bindings.add(getOptionalFirstCellWidth(), Bindings.add(getOtherCellsWidth(), getOptionalLastCellWidth()));
	protected final ObservableIntegerValue otherRowsNumber = Bindings.createIntegerBinding(() -> getElements().size(), getElements());
	protected Property<Row> selectedRow = new SimpleObjectProperty<>();

	protected ObservableValue<Row> getReferenceRow() {
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

	protected ObservableNumberValue getOptionalFirstRowHeight() {
		return Bindings.multiply(firstRowNumber, (ObservableNumberValue) firstRowHeight);
	}

	protected ObservableNumberValue getOtherRowsHeight() {
		return Bindings.multiply(getElements().size(), (ObservableNumberValue) rowHeight);
	}

	protected ObservableNumberValue getOptionalFirstCellWidth() {
		// if (getParent() instanceof Cell)
		// return Bindings.multiply(firstCellNumber, (ObservableNumberValue) ((Table) getParent().getParent()).firstColumnWidth);
		return Bindings.multiply(firstCellNumber, (ObservableNumberValue) firstColumnWidth);
	}

	protected ObservableNumberValue getOtherCellsWidth() {

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
		if (getParent() instanceof Cell) {
			if (otherRowsNumber.get() > 0)
				rowHeight.setValue(((Table) getParent().getParent().getParent()).rowHeight.getValue().intValue() - 4);
		}
		return rowHeight;
	}

	public Property<Number> getFirstColumnWidth() {
		if (getParent() instanceof Cell)
			if (otherCellsNumber.intValue() == 0)
				firstColumnWidth.setValue(((Table) getParent().getParent().getParent()).columnWidth.getValue().intValue() - 4);
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
		if (getParent() instanceof Cell)
			return new SimpleObjectProperty<Number>(((Table) getParent().getParent().getParent()).columnWidth.getValue());
		return getTableWidth();// new SimpleObjectProperty<Number>(900);
	}

	public ObservableValue<Number> getParentHeight() {
		if (getParent() instanceof Cell)
			return new SimpleObjectProperty<Number>(((Table) getParent().getParent().getParent()).rowHeight.getValue());
		return getTableHeight();// new SimpleObjectProperty<Number>(900);
	}

	// *****************************************************************************************************************************
	public static class FirstColumnTable extends Table {
		public FirstColumnTable(ObservableValue<Row> firstRow, ObservableList<Row> rows, ObservableValue<String> tableStyle) {
			super(firstRow, rows, tableStyle);
		}

		@Override
		public Property<Number> getFirstColumnWidth() {
			if (getParent() instanceof Cell)
				if (otherCellsNumber.getValue().intValue() == 0)
					firstColumnWidth.setValue(((Table) getParent().getParent().getParent()).firstColumnWidth.getValue());
			firstColumnWidth.setValue(firstColumnWidth.getValue().intValue() - 4);
			return firstColumnWidth;
		}
	}
}
