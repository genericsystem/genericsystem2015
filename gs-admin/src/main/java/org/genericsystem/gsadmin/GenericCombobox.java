package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

public class GenericCombobox extends Model {

	private Generic item;
	private Property<Generic> selectedItem = new SimpleObjectProperty<>();

	public GenericCombobox(Generic item) {
		this.item = item;
	}

	public Property<ObservableList<?>> getItems() {
		return new SimpleObjectProperty<>(item.getObservableInstances());
	}

	public Property<Generic> getSelectedItem() {
		return selectedItem;
	}

}
