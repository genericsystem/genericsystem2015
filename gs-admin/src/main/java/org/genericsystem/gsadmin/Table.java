package org.genericsystem.gsadmin;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.Listable;

public class Table extends Listable<Row> {

	private final Property<Number> rowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> firstRowHeight = new SimpleIntegerProperty(20);
	private final Property<Number> columnWidth = new SimpleIntegerProperty(80);
	private final Property<Number> firstColumnWidth = new SimpleIntegerProperty(80);
	
	private ObjectProperty<Row> rowSelected = new SimpleObjectProperty<>();
	
	private final ObservableValue<Row> referenceRow = Bindings.createObjectBinding(()-> getReferenceRow().getValue(), getFirstElement(),getElements());
	private final ObservableIntegerValue firstRowNumber = Bindings.createIntegerBinding(() -> getFirstElement().getValue() != null ? 1 : 0, getFirstElement());
	private final ObservableValue<Number> tableHeight = Bindings.add(getOptionalFirstRowHeight(), getOtherRowsHeight());
	private final ObservableIntegerValue firstCellNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getFirstElement().getValue() != null ? 1 : 0 :0, referenceRow);
	private final ObservableIntegerValue otherCellsNumber = Bindings.createIntegerBinding(() -> referenceRow.getValue() != null ? referenceRow.getValue().getElements().size() : 0, referenceRow);
	private final ObservableValue<Number> tableWidth = Bindings.add(getOptionalFirstCellWidth(), getOtherCellsWidth());
	
	private ObservableValue<Row> getReferenceRow(){
		if(getFirstElement()!=null){
			return getFirstElement();
		}
		else if(getElements().size()!=0)
		{	
			return new SimpleObjectProperty<>(getElements().get(0));
		}
		else{
			return new SimpleObjectProperty<>();
		}
	}
	
	public ObjectProperty<Row> getRowSelected() {
		return rowSelected;
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

	// private ObservableList<Cell<?>> getCells() {
	// return getFirstElement().getValue().getElements();
	// }

	// private final ObservableValue<Number> tableWidth = Bindings.add(Bindings.multiply(getWidth(), (ObservableNumberValue) columnWidth), getWidthFirstRow());
	// private final ObservableValue<Number> tableHeight = Bindings.add(Bindings.multiply(getElements().size(), (ObservableNumberValue) rowHeight), getHeightFirstRow());

	// private ObservableNumberValue getWidth() {
	// ObservableBooleanValue isFirstElementNull = Bindings.createBooleanBinding(() -> getFirstElement().getValue() == null, new SimpleObjectProperty<>(getFirstElement().getValue()));
	// return new SimpleIntegerProperty(Bindings.when(isFirstElementNull).then(getElements().size()).otherwise(!isFirstElementNull.getValue() ? getFirstElement().getValue().getElements().size() : getElements().size() - 1).getValue().intValue());
	// }
	//
	// private ObservableNumberValue getWidthFirstRow() {
	// return getFirstElement().getValue() != null ? (ObservableNumberValue) firstColumnWidth : new SimpleIntegerProperty(1);
	// }
	//
	// private ObservableNumberValue getHeightFirstRow() {
	// return getFirstElement().getValue() != null ? (ObservableNumberValue) firstRowHeight : new SimpleIntegerProperty(1);
	// }

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

	public void selectRow(Row row) {
		rowSelected.setValue(row);
		System.out.println(row.getItem());
	}
}
