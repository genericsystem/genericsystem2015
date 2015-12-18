package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.Transformation;

public class AttributeWrapper {
	Generic attribute;
	private StringProperty stringProperty = new SimpleStringProperty();
	Transformation<HolderWrapper, Generic> holdersValue;

	public AttributeWrapper(Generic att, Generic generic) {
		this.attribute = att;
		stringProperty.set(attribute.getValue().toString());
		holdersValue = new Transformation<HolderWrapper, Generic>(FXCollections.observableArrayList(generic.getHolders(attribute).toList()), holder -> new HolderWrapper(holder));
	}

	public StringProperty getObservable() {
		return stringProperty;
	}

	public ObservableList<HolderWrapper> getHoldersObservableList() {
		return holdersValue;
	}

	public static void init(Element<VBox> parent) {
		GSHBox titleColumnPanel = new GSHBox(parent).setStyleClass("header");
		{
			new GSLabel(titleColumnPanel, "").setPrefWidth(100);
			new GSLabel(titleColumnPanel, AttributeWrapper::getObservable).setPrefWidth(100).forEach(TypeWrapper::getAttributeTitle);
		}
	}
}
