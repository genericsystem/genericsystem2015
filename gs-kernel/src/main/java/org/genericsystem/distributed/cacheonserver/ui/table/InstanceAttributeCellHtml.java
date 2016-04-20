package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlSection;

public class InstanceAttributeCellHtml extends HtmlSection {

	public InstanceAttributeCellHtml(InstanceRowHtml parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HolderSubCellHtml(this).forEach(InstanceAttributeCellModel::getHolderModels);
	}
}
