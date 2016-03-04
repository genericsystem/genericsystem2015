package org.genericsystem.ui.components;

import java.util.List;
import java.util.function.Function;
import javafx.scene.layout.VBox;
import org.genericsystem.distributed.ui.Element;

public class GSVBox extends GSPane<GSVBox, VBox> {

	public GSVBox(Element<?> parent) {
		super(parent, VBox.class);
	}

	public <PARENTNODE> GSVBox(Element<?> parent, Function<? super PARENTNODE, List<?>> getGraphicChildren) {
		super(parent, VBox.class, getGraphicChildren);
	}

	public GSVBox setSpacing(Number value) {
		addBoot(VBox::spacingProperty, value);
		return this;
	}

}
