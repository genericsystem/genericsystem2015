package org.genericsystem.ui.table;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import org.genericsystem.distributed.ui.Model;

public abstract class Crud extends Model {
	protected final Property<Table> table;
	protected final Property<Table> editTable = new SimpleObjectProperty<>();
	protected final StringProperty name = new SimpleStringProperty();

	public Crud(Property<Table> table) {
		this.table = table;
	}

	public Property<Table> getEditTable() {
		return editTable;
	}

	public StringProperty getName() {
		return name;
	}

	public Property<Table> getTable() {
		return table;
	}

	public abstract <T> T getModel();

	public abstract void add();

}
