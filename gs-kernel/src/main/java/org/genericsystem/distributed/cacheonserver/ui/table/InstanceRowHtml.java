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
		new HtmlLabel(new HtmlSection(this).addStyleClass("gscell")).bindText(InstanceRowModel::getString);
		new InstanceAttributeCellHtml(this).forEach(InstanceRowModel::getInstanceAttributeModels);
		new HtmlButton(new HtmlSection(this).addStyleClass("gscell")).bindAction(InstanceRowModel::remove).setText("Remove");
	}
}
