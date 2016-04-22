package org.genericsystem.distributed.cacheonserver.ui.table.title;

import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TitleTypeTableHtml extends TypeTableHtml {
	public TitleTypeTableHtml(HtmlElement<?, ?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new HtmlH1(new HtmlSection(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TitleTypeTableModel::getTableString);
		new TitleRowHtml(this).select(TitleTypeTableModel::getTitleRowModel);
		new InstanceRowHtml(this).forEach(TitleTypeTableModel::getSubModels);
	}

}
