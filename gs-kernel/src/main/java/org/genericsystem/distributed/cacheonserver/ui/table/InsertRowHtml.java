package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InsertRowHtml extends HtmlSection {

	public InsertRowHtml(TypeTableHtml parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlInputText(new HtmlSection(this).addStyleClass("gscell")).bindTextBidirectional(InsertRowModel::getInputString);
		new AttributeCellHtml(this).forEach(InsertRowModel::getAttributeCellModels);
		new HtmlButton(new HtmlSection(this).addStyleClass("gscell")).bindAction(InsertRowModel::create).setText("Create");
	}

}
