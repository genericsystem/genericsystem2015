package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.control.ScrollPane;

import org.genericsystem.ui.Element;

public class GSSCrollPane extends Element<ScrollPane> {

	public GSSCrollPane(Element<?> parent) {
		super(parent, ScrollPane.class);
	}

	public <PARENTNODE> GSSCrollPane(Element<PARENTNODE> parent, Function<PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, ScrollPane.class, getGraphicChildren);
	}

}
