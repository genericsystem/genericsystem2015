package org.genericsystem.reactor.ca_gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ba_htmltag.HtmlButton;

public class GSModalButton extends HtmlButton {

	public GSModalButton(Tag parent, GSDiv gSection) {
		super(parent);
		bindAction(model -> gSection.getDisplayProperty(model).setValue("flex"));
	}
}
