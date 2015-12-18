package org.genericsystem.gsadmin;

import javafx.collections.FXCollections;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;

public class AttributeWrapper extends AbstractGenericWrapper {
	public AttributeWrapper(Generic attribute, Generic generic) {
		super(attribute, gene -> FXCollections.observableArrayList(generic.getHolders(attribute).toList()), holder -> new HolderWrapper(holder));
	}

	public static void init(Element<VBox> parent) {
		GSHBox titleColumnPanel = new GSHBox(parent).setStyleClass("header");
		{
			new GSLabel(titleColumnPanel, TypeWrapper::getObservableText).setPrefWidth(100);
			new GSLabel(titleColumnPanel, AttributeWrapper::getObservableText).setPrefWidth(100).forEach(TypeWrapper::getAttributeTitle);
		}
	}
}
