package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InsertAttributeCellHtml extends HtmlSection {

	public InsertAttributeCellHtml(InsertRowHtml parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlInputText(this).bindTextBidirectional(InsertAttributeCellModel::getInputString);
	}
}
