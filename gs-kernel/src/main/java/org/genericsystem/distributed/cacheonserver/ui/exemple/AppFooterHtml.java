package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

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