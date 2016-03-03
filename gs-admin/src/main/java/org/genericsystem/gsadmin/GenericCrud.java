package org.genericsystem.gsadmin;

import java.io.Serializable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.util.converter.DoubleStringConverter;
import javafx.util.converter.IntegerStringConverter;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.utils.Transformation;
import org.genericsystem.ui.table.Crud;
import org.genericsystem.ui.table.Table;

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
		generic.addInstance(getInstanceValue(), new Transformation<Generic, GenericCombobox>(listCombobox, combo -> combo.getSelectedItem().getValue()).toArray(new Generic[listCombobox.size()]));
	}

	private Serializable getInstanceValue() {
		if (Integer.class.equals(generic.getInstanceValueClassConstraint()))
			return new IntegerStringConverter().fromString(name.getValue());

		if (Double.class.equals(generic.getInstanceValueClassConstraint()))
			return new DoubleStringConverter().fromString(name.getValue());

		return name.getValue();
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
