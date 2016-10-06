package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

public class StepNavigator extends GSDiv {

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