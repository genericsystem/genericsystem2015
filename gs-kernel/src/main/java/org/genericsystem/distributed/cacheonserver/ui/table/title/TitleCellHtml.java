package org.genericsystem.distributed.cacheonserver.ui.table.title;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlStrong;
import org.genericsystem.distributed.ui.models.GenericModel;

class TitleCellHtml extends HtmlSection {

	public TitleCellHtml(TitleRowHtml parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(new HtmlStrong(this)).bindText(GenericModel::getString);
	}
}
