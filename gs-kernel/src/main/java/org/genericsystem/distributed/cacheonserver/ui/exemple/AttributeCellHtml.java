package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class AttributeCellHtml extends HtmlSection {

	public AttributeCellHtml(InsertRowHtml parent) {
		super(parent);
		setStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlInputText(this).bindTextBidirectional(AttributeCellModel::getInputString);
	}
}
