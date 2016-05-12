package org.genericsystem.ui.components;

import javafx.scene.layout.VBox;
import org.genericsystem.reactor.Element;

public class GSVBox extends GSPane<GSVBox, VBox> {

	public GSVBox(Element<?> parent) {
		super(parent, VBox.class);
	}

	public GSVBox setSpacing(Number value) {
		addBoot(VBox::spacingProperty, value);
		return this;
	}
}
