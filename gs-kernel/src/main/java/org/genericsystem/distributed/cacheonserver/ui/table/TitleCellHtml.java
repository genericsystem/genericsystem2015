package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlStrong;

public class TitleCellHtml extends HtmlSection {

	public TitleCellHtml(TitleRowHtml parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(new HtmlStrong(this)).bindText(TitleCellModel::getString);
	}
}
