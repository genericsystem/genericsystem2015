package org.genericsystem.gsadmin;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
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

	public TypeWrapper(Generic generic) {
		super(generic, g -> generic.getObservableSubInstances(), gen -> new InstanceWrapper(gen, generic));

		ObservableList<Generic> atts = generic.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generic));
		attributeTitle = new Transformation<AttributeWrapper, Generic>(atts, att -> new AttributeWrapper(att, generic));
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
