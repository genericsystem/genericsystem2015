package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.cacheonserver.ui.table.title.insertable.InsertRowHtml;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class AttributeCellHtml extends HtmlSection {

	public AttributeCellHtml(InsertRowHtml parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlInputText(this).bindTextBidirectional(AttributeCellModel::getInputString);
	}
}
