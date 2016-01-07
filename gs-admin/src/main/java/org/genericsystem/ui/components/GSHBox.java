package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;

public class GSHBox extends GSPane<GSHBox, HBox> {

	public GSHBox(Element<?> parent) {
		super(parent, HBox.class);
	}

	public <PARENTNODE> GSHBox(Element<?> parent, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, HBox.class, getGraphicChildren);
	}

	public GSHBox setSpacing(Number value) {
		addBoot(HBox::spacingProperty, value);
		return this;
	}

}
