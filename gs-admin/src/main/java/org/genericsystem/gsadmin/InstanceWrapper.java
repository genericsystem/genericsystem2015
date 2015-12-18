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
import org.genericsystem.ui.components.GSVBox;
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

		GSVBox mainPanel = new GSVBox(parent).include(AttributeWrapper::init);
		{
			GSHBox rowPanel = new GSHBox(mainPanel).setPrefWidth(100).forEach(TypeWrapper::getInstanceWrapperList);
			{
				new GSLabel(rowPanel, InstanceWrapper::getObservable).setPrefWidth(100).setStyleClass("columnInstance");
				new GSVBox(rowPanel).forEach(InstanceWrapper::getAttributeObservableList).setPrefWidth(100).include(HolderWrapper::init).setStyleClass("cell");
			}

		}
	}
}
