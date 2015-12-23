package org.genericsystem.gsadmin.old;

import javafx.collections.FXCollections;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.AbstractGenericWrapper;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;

public class AttributeWrapper extends AbstractGenericWrapper {
	public AttributeWrapper(Generic attribute, Generic generic, Boolean isEngine) {
		super(attribute, gene -> FXCollections.observableArrayList(generic.getHolders(attribute).toList()), holder -> new HolderWrapper(holder, isEngine));

		System.out.println(generic.getValue());
		if (isEngine) {
			System.out.println(generic.getHolders(attribute).size());
		}
	}

	public static void init(Element<VBox> parent) {
		GSHBox titleColumnPanel = new GSHBox(parent).setStyleClass("header");
		{
			new GSLabel(titleColumnPanel, TypeWrapper::getObservableText).setPrefWidth(100);
			new GSLabel(titleColumnPanel, AttributeWrapper::getObservableText).setPrefWidth(100).forEach(TypeWrapper::getAttributeTitle);
		}
	}

}
