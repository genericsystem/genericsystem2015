package org.genericsystem.gsadmin;

import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;

public class HolderWrapper extends AbstractGenericWrapper {

	public HolderWrapper(Generic holder, Boolean isEngine) {
		super(holder);
	}

	public static void init(Element<VBox> parent) {
		new GSLabel(parent, HolderWrapper::getObservableText).setPrefWidth(100).forEach(AttributeWrapper::getObservableListWrapper);
	}
}
