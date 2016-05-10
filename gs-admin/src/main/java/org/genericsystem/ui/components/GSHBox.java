package org.genericsystem.ui.components;

import java.util.function.Consumer;
import javafx.scene.layout.HBox;
import org.genericsystem.reactor.Element;

public class GSHBox extends GSPane<GSHBox, HBox> {

	public GSHBox(Element<?> parent) {
		super(parent, HBox.class);
	}

	public GSHBox setSpacing(Number value) {
		addBoot(HBox::spacingProperty, value);
		return this;
	}

	public <M> GSHBox addOnClick(Consumer<M> applyOnModel) {
		addActionBinding(HBox::onMouseClickedProperty, applyOnModel);
		return this;
	}

}
