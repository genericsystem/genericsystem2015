package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class HolderSubCellHtml extends HtmlSection {

	public HolderSubCellHtml(InstanceAttributeCellHtml parent) {
		super(parent);
		setStyleClass("gssubcell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(this).bindText(HolderSubCellModel::getString);
	}
}
