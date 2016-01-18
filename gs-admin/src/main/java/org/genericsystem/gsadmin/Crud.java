package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;
import org.genericsystem.ui.table.Table;

public abstract class Crud {
	protected final Property<Table> table;
	protected Property<Table> editTable = new SimpleObjectProperty<Table>();
	protected StringProperty name = new SimpleStringProperty();

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

	public static class GenericCrud extends Crud {
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
			((CocCache) generic.getCurrentCache()).shiftTs();
		}

		public void cancel() {
			((CocCache) generic.getCurrentCache()).clear();
		}

		public void mount() {
			((CocCache) generic.getCurrentCache()).mount();
		}

		public void unmount() {
			((CocCache) generic.getCurrentCache()).unmount();
		}
	}
}
