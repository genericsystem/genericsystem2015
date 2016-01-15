package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.ui.table.Table;

public abstract class Crud {
	protected final Property<Table> table;
	protected StringProperty name = new SimpleStringProperty();
	
	
	public Crud(Property<Table> table) {
		this.table = table;
	}
	
	public StringProperty getName() {
		return name;
	}
	
	public Property<Table> getTable() {
		return table;
	}
	
	public abstract<T> T getModel();
	public abstract void add();
	
	public static class EngineCrud extends Crud{
		private final CocClientEngine engine;
		public EngineCrud(CocClientEngine engine, Property<Table> table) {
			super(table);
			this.engine = engine;
		}
		
		public CocClientEngine getEngine() {
			return engine;
		}
		@Override
		public void add(){
			engine.addInstance(name.getValue());
		}
		
		public void flush(){
			engine.getCurrentCache().flush();
		}
		
		public void shiftTs(){
			engine.getCurrentCache().shiftTs();
		}
		
		public void cancel(){
			engine.getCurrentCache().clear();
		}
		
		public void mount(){
			engine.getCurrentCache().mount();
		}
		
		public void unmount(){
			engine.getCurrentCache().unmount();
		}

		@Override
		public <T> T getModel() {
			return (T) engine;
		}
	}
	
	public static class GenericCrud extends Crud{
		private final Generic generic;
		public GenericCrud(Property<Table> table,Generic generic) {
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
	}
}
