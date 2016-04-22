package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.GenericModel;

public class HolderSubCellHtml extends HtmlSection {

	public HolderSubCellHtml(InstanceAttributeCellHtml parent) {
		super(parent);
		addStyleClass("gssubcell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(this).bindText(GenericModel::getString);
	}
}
