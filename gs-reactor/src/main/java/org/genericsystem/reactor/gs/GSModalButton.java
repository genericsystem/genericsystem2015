package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlButton;

public class GSModalButton extends HtmlButton {

	public GSModalButton(Tag parent, GSSection gSection) {
		super(parent);
		bindAction(model -> gSection.getDisplayProperty(model).setValue("flex"));
	}
}
