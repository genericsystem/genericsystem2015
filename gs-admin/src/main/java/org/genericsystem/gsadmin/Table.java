package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.Listable;

public class Table extends Listable<Row> {

	public Table(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
		super(tableStyle.table, firstRow, rows);
	}
}
