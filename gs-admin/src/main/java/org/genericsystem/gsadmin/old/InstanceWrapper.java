package org.genericsystem.gsadmin.old;

import javafx.collections.FXCollections;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public class InstanceWrapper extends AbstractGenericWrapper {

	public InstanceWrapper(Generic instance, Generic type) {
		super(instance, gen -> FXCollections.observableArrayList(type.getAttributes().filter(attribute -> attribute.isCompositeForInstances(type)).toList()), att -> new AttributeWrapper(att, instance, false));
	}

	public static void init(Element<HBox> parent) {
		GSVBox mainPanel = new GSVBox(parent).include(AttributeWrapper::init);
		{
			GSHBox rowPanel = new GSHBox(mainPanel).forEach(TypeWrapper::getObservableListWrapper);
			{
				new GSLabel(rowPanel, InstanceWrapper::getObservableText).setPrefWidth(100).setStyleClass("columnInstance");
				new GSVBox(rowPanel).forEach(InstanceWrapper::getObservableListWrapper).setPrefWidth(100).setStyleClass("cell").include(HolderWrapper::init);
			}
		}
	}
}
