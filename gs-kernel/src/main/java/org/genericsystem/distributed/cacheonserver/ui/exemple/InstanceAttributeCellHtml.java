package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InstanceAttributeCellHtml extends HtmlSection {

	public InstanceAttributeCellHtml(InstanceRowHtml parent) {
		super(parent);
		setStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(this).bindText(HolderSubCellModel::getString).forEach(InstanceAttributeCellModel::getHolderModels);
	}
}
