package org.genericsystem.example.reactor;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlSection;

public class AppFooterHtml extends HtmlSection<AppModel> {
	public AppFooterHtml(HtmlElement<?, ?> parent) {
		super(parent);
		addStyleClass("gsfooter");
		new HtmlH1<AppModel>(this).setText("Footer");
	}
}