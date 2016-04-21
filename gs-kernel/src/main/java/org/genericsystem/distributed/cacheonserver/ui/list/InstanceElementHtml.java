package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.GenericModel;

public class InstanceElementHtml extends HtmlSection {

	public InstanceElementHtml(TypeListHtml parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(new HtmlSection(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(GenericModel::getString);
	}
}
