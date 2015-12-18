package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public class HolderWrapper {
	Generic holder;
	private StringProperty stringProperty = new SimpleStringProperty();

	public HolderWrapper(Generic hold) {
		this.holder = hold;
		stringProperty.set(hold.getValue().toString());
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public static void init(Element<VBox> parent) {
		GSVBox vbHolder = new GSVBox(parent).forEach(AttributeWrapper::getHoldersObservableList);
		{
			new GSLabel(vbHolder, HolderWrapper::getObservable).setPrefWidth(100);
		}
	}
}
