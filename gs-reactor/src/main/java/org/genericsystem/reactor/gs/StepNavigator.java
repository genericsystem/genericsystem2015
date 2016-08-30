package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlHyperLink;

public class StepNavigator extends GSSection {

	public StepNavigator(Tag parent, FlexDirection direction) {
		super(parent, direction);
		addStyle("justify-content", "space-between");
		new HtmlHyperLink(this) {
			{
				setText("<");
				bindAction(model -> prev(model));
			}
		};
		new HtmlHyperLink(this) {
			{
				setText(">");
				bindAction(model -> next(model));
			}
		};
	}
}