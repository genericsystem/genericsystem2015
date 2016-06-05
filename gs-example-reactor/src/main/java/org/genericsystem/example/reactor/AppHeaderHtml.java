package org.genericsystem.example.reactor;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlSection;

public class AppHeaderHtml extends HtmlSection<AppModel> {
	public AppHeaderHtml(HtmlElement<?, ?> parent) {
		super(parent);
		addStyleClass("gsheader");
		new HtmlH1<AppModel>(this).setText("Reactive System Live Demo");
	}
}
