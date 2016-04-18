package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InstanceRowHtml extends HtmlSection {

	public InstanceRowHtml(TypeTableHtml parent) {
		super(parent);
		setStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel(this).bindText(InstanceRowModel::getString);
		new HtmlSection(this).forEach(InstanceRowModel::getInstanceAttributeModels);
		new HtmlButton(this).setStyleClass("destroy").bindAction(InstanceRowModel::remove);
	}
}
