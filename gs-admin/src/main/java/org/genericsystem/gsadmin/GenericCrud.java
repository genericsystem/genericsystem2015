package org.genericsystem.gsadmin;

import javafx.beans.property.Property;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Crud;
import org.genericsystem.ui.table.Table;

public class GenericCrud extends Crud {

	private final Generic generic;

	public GenericCrud(Property<Table> table, Generic generic) {
		super(table);
		this.generic = generic;
	}

	@Override
	public void add() {
		generic.addInstance(name.getValue());
	}

	@Override
	public <T> T getModel() {
		return (T) generic;
	}

	public void flush() {
		generic.getCurrentCache().flush();
	}

	public void shiftTs() {
		generic.getCurrentCache().shiftTs();
	}

	public void cancel() {
		generic.getCurrentCache().clear();
	}

	public void mount() {
		generic.getCurrentCache().mount();
	}

	public void unmount() {
		generic.getCurrentCache().unmount();
	}
}
