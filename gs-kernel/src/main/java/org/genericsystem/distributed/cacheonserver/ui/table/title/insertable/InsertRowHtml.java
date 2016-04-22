package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InsertRowHtml extends HtmlSection {

	public InsertRowHtml(TypeTableHtml parent) {
		super(parent);
		addStyleClass("gsrow gsinsertrow");
	}

	@Override
	protected void initChildren() {
		new HtmlInputText(new HtmlSection(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindTextBidirectional(InsertRowModel::getInputString);
		new InsertAttributeCellHtml(this).forEach(InsertRowModel::getSubModels);
		new HtmlButton(new HtmlSection(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(InsertRowModel::create).setText("Create");
	}

}
