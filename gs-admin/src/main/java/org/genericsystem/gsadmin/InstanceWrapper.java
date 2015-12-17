package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.Transformation;

public class InstanceWrapper {
	Generic instance;
	private StringProperty stringProperty = new SimpleStringProperty();
	Transformation<AttributeWrapper, Generic> attributeObservableList;

	public InstanceWrapper(Generic inst, Generic type) {
		this.instance = inst;
		stringProperty.set(instance.getValue().toString());
		attributeObservableList = new Transformation<AttributeWrapper, Generic>(FXCollections.observableArrayList(type.getAttributes().filter(attribute -> attribute.isCompositeForInstances(type)).toList()), att -> new AttributeWrapper(att, instance));
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public ObservableList<AttributeWrapper> getAttributeObservableList() {
		return attributeObservableList;
	}

	public static void init(Element<HBox> parent) {

		GSHBox hb = new GSHBox(parent).setSpacing(100);
		{
			new GSLabel(hb, AttributeWrapper::getObservable).forEach(TypeWrapper::getAttributeTitle).setPrefWidth(80);
		}

		// GSHBox hb = new GSHBox(parent).forEach(TypeWrapper::getInstanceWrapperList).setSpacing(100);
		// {
		// GSHBox hb2 = new GSHBox(hb).forEach(InstanceWrapper::getAttributeObservableList);
		// {
		// new GSLabel(hb2, AttributeWrapper::getObservable).setPrefWidth(80);
		// }
		// }
	}
}
