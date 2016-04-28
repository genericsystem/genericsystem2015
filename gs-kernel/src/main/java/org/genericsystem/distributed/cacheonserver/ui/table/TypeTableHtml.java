package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TypeTableHtml<M extends TypeTableModel> extends HtmlSection<M> {

	public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new HtmlH1<M>(new HtmlSection<>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TypeTableModel::getString);
		new InstanceRowHtml<>(this).forEach(TypeTableModel::getSubModels);
	}
}
