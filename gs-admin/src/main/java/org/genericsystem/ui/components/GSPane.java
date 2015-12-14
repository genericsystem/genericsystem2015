package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

import org.genericsystem.ui.Element;

public class GSPane<Component extends GSPane<Component, N>, N extends Pane> extends GSRegion<Component, N> {

	public GSPane(Element parent, Class<N> paneClass) {
		super(parent, paneClass);
	}

	public <PARENTNODE> GSPane(Element parent, Class<N> paneClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

}
