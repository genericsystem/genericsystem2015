package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.HtmlButton;

public class GSModalButton extends HtmlButton {

	public GSModalButton(GSTag parent, GSSection gSection) {
		super(parent);
		bindAction(model -> gSection.getProperty(ReactorStatics.DISPLAY, model).setValue("flex"));
	}
}
