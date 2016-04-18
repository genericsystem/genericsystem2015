package org.genericsystem.distributed.cacheonserver.ui.exemple;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TypeTableHtml extends HtmlSection {

	public TypeTableHtml(HtmlElement<?, ?> parent) {
		super(parent);
		setStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new InstanceRowHtml(this).forEach(TypeTableModel::getInstanceModels);
	}
}
