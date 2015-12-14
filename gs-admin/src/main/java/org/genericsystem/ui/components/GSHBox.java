package org.genericsystem.ui.components;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;

public class GSHBox extends GSPane<GSHBox, HBox> {

	public GSHBox(Element parent) {
		super(parent, HBox.class);
	}

	@Override
	public <M, T> GSHBox addForEachMetaBinding(Function<M, ObservableList<T>> function, Function<T, Property<M>> injectedProperty, Consumer<Element<HBox>> subModelInit) {
		super.addForEachMetaBinding(function, injectedProperty, subModelInit);
		return this;
	}

}
