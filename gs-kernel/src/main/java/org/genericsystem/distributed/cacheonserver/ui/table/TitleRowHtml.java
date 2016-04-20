package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlStrong;

public class TitleRowHtml extends HtmlSection {

	public TitleRowHtml(TypeTableHtml parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(new HtmlStrong(new HtmlSection(this).addStyleClass("gscell"))).bindText(TitleRowModel::getFirstCellString);
		new TitleCellHtml(this).forEach(TitleRowModel::getTitleCellModels);
		new HtmlSection(this).addStyleClass("gscell");
	}
}
