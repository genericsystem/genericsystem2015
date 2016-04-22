package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InstanceRowHtml extends HtmlSection {

	public InstanceRowHtml(TypeTableHtml parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(new HtmlSection(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(InstanceRowModel::getString);
		new InstanceAttributeCellHtml(this).forEach(InstanceRowModel::getSubModels);
		new HtmlButton(new HtmlSection(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(InstanceRowModel::remove).setText("Remove");
	}
}
