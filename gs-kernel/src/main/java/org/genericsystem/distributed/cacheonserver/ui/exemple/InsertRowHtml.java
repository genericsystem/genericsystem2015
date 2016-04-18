package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InsertRowHtml extends HtmlSection {

	public InsertRowHtml(TypeTableHtml parent) {
		super(parent);
		setStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		HtmlSection firstCell = new HtmlSection(this).setStyleClass("gscell");
		new HtmlInputText(firstCell).bindTextBidirectional(InsertRowModel::getInputString);
		new AttributeCellHtml(this).forEach(InsertRowModel::getAttributeCellModels);
		new HtmlButton(this).bindAction(InsertRowModel::create).setText("Create").setStyleClass("gscell");

	}

}
