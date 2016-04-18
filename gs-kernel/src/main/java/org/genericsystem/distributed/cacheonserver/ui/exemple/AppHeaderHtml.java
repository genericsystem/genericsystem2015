package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class AppHeaderHtml extends HtmlSection {
	public AppHeaderHtml(AppHtml appHtml) {
		super(appHtml);
	}

	@Override
	protected void initChildren() {
		new HtmlH1(this).setText("Cars");
		// new HtmlInputText(header).setStyleClass("new-todo").bindTextBidirectional(AppModel::getCarString);
		// new HtmlInputText(header).setStyleClass("new-todo").bindAction(AppModel::create).bindTextBidirectional(AppModel::getPowerString);

	}
}
