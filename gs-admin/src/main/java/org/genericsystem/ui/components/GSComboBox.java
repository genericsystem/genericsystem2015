package org.genericsystem.ui.components;

import java.util.function.Function;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import org.genericsystem.reactor.Element;

public class GSComboBox<G> extends GSRegion<GSComboBox<G>, ComboBox> {
	public GSComboBox(Element<?> parent) {
		super(parent, ComboBox.class);
	}

	public GSComboBox(Element<?> parent, Function<G, ObservableValue<ObservableList<?>>> options) {
		super(parent, ComboBox.class);
		this.addBinding(ComboBox::itemsProperty, options);
	}

	@Override
	public <M, T> Element<ComboBox> addReversedBinding(Function<ComboBox, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		return super.addReversedBinding(applyOnNode, applyOnModel);
	}

}
