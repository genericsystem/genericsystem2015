package org.genericsystem.gsadmin;

import javafx.beans.property.Property;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Model;
import org.genericsystem.ui.table.Crud;
import org.genericsystem.ui.table.Table;

public class GenericCrud extends Crud {

	private Generic generic;

	public void test(GenericRow row) {
		System.out.println("test action");
	}

	public GenericCrud() {
		super(null, null);
	}

	public GenericCrud(Model parent, Property<Table> table, Generic generic) {
		super(parent, table);
		this.generic = generic;
	}

	public void setModel(Generic gen) {
		this.generic = gen;
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
