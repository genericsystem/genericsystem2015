package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TypeTableHtml extends HtmlSection {

	public TypeTableHtml(HtmlElement<?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new HtmlH1(new HtmlSection(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TypeTableModel::getTableString);
		new InstanceRowHtml(this).forEach(TypeTableModel::getSubModels);
	}
}
