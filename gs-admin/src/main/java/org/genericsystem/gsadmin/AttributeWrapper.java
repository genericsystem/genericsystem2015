package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.utils.Transformation;

public class AttributeWrapper {
	Generic attribute;
	private StringProperty stringProperty = new SimpleStringProperty();
	Transformation<HolderWrapper, Generic> holdersValue;

	public AttributeWrapper(Generic att, Generic generic) {
		this.attribute = att;
		stringProperty.set(attribute.getValue().toString());
		// System.out.println(stringProperty.get());
		holdersValue = new Transformation<HolderWrapper, Generic>(FXCollections.observableArrayList(generic.getHolders(attribute).toList()), holder -> new HolderWrapper(holder));
	}

	public StringProperty getObservable() {
		return stringProperty;
	}

	public ObservableList<HolderWrapper> getHoldersObservableList() {
		return holdersValue;
	}
}
