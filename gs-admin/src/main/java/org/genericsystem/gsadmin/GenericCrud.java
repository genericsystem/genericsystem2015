package org.genericsystem.gsadmin;

import java.io.Serializable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Crud;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public class GenericCrud extends Crud {

	private final Generic generic;
	private final ObservableList<GenericCombobox> listCombobox = FXCollections.observableArrayList();

	public void test(GenericRow row) {
		System.out.println("test action");
	}

	public ObservableList getObservablelist() {
		return FXCollections.observableArrayList(generic.getComponents());
	}

	public ObservableValue<ObservableList<?>> getObservable() {
		return new SimpleObjectProperty<>(FXCollections.observableArrayList(generic.getComponents()));
	}

	public ObservableList<GenericCombobox> getListCombobox() {
		return listCombobox;
	}

	public GenericCrud(Property<Table> table, Generic generic) {
		super(table);
		this.generic = generic;
		generic.getComponents().forEach(component -> listCombobox.add(new GenericCombobox(component)));
	}

	@Override
	public void add() {
		ObservableList<Generic> list = new Transformation<Generic, GenericCombobox>(listCombobox, combo -> combo.getSelectedItem().getValue());
		generic.addInstance(converter(name.getValue()), list.toArray(new Generic[listCombobox.size()]));
	}

	private Serializable converter(String valueToConvert) {
		if (Integer.class.equals(generic.getInstanceValueClassConstraint()))
			return Integer.parseInt(name.getValue());

		if (Double.class.equals(generic.getInstanceValueClassConstraint()))
			return Double.parseDouble(name.getValue());

		return valueToConvert;
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
