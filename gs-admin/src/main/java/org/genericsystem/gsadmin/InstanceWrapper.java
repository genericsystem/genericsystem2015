package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.utils.Transformation;

public class InstanceWrapper {
	Generic instance;
	private StringProperty stringProperty = new SimpleStringProperty();
	Transformation<AttributeWrapper, Generic> attributeObservableList;

	public InstanceWrapper(Generic inst) {
		this.instance = inst;
		stringProperty.set(instance.getValue().toString());
		attributeObservableList = new Transformation<AttributeWrapper, Generic>(FXCollections.observableArrayList(instance.getAttributes().toList()), att -> new AttributeWrapper(att, instance));
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public ObservableList<AttributeWrapper> getAttributeObservableList() {
		return attributeObservableList;
	}

	public static void init(Element<HBox> parent) {
		// GSHBox hb = new GSHBox(parent).forEach(TypeWrapper::getInstanceWrapperList).setSpacing(100);
		// {
		// GSHBox hb2 = new GSHBox(hb).forEach(InstanceWrapper::getAttributeObservableList);
		// {
		// new GSLabel(hb2, AttributeWrapper::getObservable).setPrefWidth(80);
		// }
		// }
	}
}
