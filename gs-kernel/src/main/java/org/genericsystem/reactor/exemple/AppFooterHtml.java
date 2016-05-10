package org.genericsystem.reactor.exemple;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlSection;

public class AppFooterHtml extends HtmlSection<AppModel> {
	public AppFooterHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gsfooter");
	}

	@Override
	protected void initChildren() {
		new HtmlH1<AppModel>(this).setText("Footer");
	}
}