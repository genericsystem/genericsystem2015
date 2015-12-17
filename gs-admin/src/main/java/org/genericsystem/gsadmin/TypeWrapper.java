package org.genericsystem.gsadmin;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.Transformation;

public class TypeWrapper {
	private Generic generic;
	private StringProperty stringProperty = new SimpleStringProperty();
	private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);
	private Transformation<InstanceWrapper, Generic> instanceWrapperList;
	private Transformation<AttributeWrapper, Generic> attributeTitle;

	public TypeWrapper(Generic g) {
		this.generic = g;
		this.stringProperty.setValue(generic.getValue().toString());
		instanceWrapperList = new Transformation<InstanceWrapper, Generic>(FXCollections.observableArrayList(generic.getSubInstances().toList()), gen -> new InstanceWrapper(gen, this.generic));
		ObservableList<Generic> atts = FXCollections.observableArrayList();
		atts.add(generic);
		atts.addAll(generic.getAttributes().filter(attribute -> attribute.isCompositeForInstances(generic)).toList());
		attributeTitle = new Transformation<AttributeWrapper, Generic>(atts, att -> new AttributeWrapper(att, generic));
	}

	public ObservableList<InstanceWrapper> getInstanceWrapperList() {
		return instanceWrapperList;
	}

	public ObservableList<AttributeWrapper> getAttributeTitle() {
		return attributeTitle;
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public ObservableValue<String> getRemoveButtonTextProperty() {
		return removeButtonTextProperty;
	}

	public void remove() {
		generic.remove();
	}

	public static void init(Element<HBox> parent) {
		new GSLabel(parent, TypeWrapper::getObservable).setPrefWidth(100);
		new GSButton(parent, "remove").setAction(TypeWrapper::remove).setPrefWidth(100);
		new GSButton(parent, "select").setMetaAction((gl, gw) -> ((GenericList) gl).getSelection().setValue((TypeWrapper) gw)).setPrefWidth(90);
	}
}
