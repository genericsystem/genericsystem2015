package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.layout.VBox;

import org.genericsystem.ui.Element;

public class GSVBox extends GSPane<GSVBox, VBox> {

	public GSVBox(Element parent) {
		super(parent, VBox.class);
	}

	public <PARENTNODE> GSVBox(Element parent, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, VBox.class, getGraphicChildren);
	}
}
