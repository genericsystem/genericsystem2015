package org.genericsystem.gsadmin.old;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.Transformation;

public class TypeWrapper extends AbstractGenericWrapper {

	private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", getObservableText());
	private Transformation<AttributeWrapper, Generic> attributeTitle;

	public TypeWrapper(Generic generic, Boolean isEngine) {
		super(generic, g -> FXCollections.observableArrayList(generic.getSubInstances().toList()), gen -> isEngine ? new TypeWrapper(gen, false) : new InstanceWrapper(gen, generic));
		// problème de récupération des type à ce niveau
		ObservableList<Generic> atts = FXCollections.observableArrayList(generic.getAttributes().filter(attribute -> attribute.isCompositeForInstances(generic)).toList());
		attributeTitle = new Transformation<AttributeWrapper, Generic>(atts, att -> new AttributeWrapper(att, generic, isEngine));
	}

	public ObservableList<AttributeWrapper> getAttributeTitle() {
		return attributeTitle;
	}

	public ObservableValue<String> getRemoveButtonTextProperty() {
		return removeButtonTextProperty;
	}

	public void remove() {
		generic.remove();
	}

	public static void init(Element<HBox> parent) {

		new GSLabel(parent, TypeWrapper::getObservableText).setPrefWidth(100);
		new GSButton(parent, "remove").setAction(TypeWrapper::remove).setPrefWidth(100);
		new GSButton(parent, "select").setMetaAction((gl, gw) -> ((GenericList) gl).getSelection().setValue((TypeWrapper) gw)).setPrefWidth(90);
	}
}
