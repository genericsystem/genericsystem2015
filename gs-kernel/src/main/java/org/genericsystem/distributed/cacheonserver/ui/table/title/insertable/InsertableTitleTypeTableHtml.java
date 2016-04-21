package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableHtml;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InsertableTitleTypeTableHtml extends TitleTypeTableHtml {

	public InsertableTitleTypeTableHtml(HtmlElement<?, ?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new HtmlH1(new HtmlSection(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(InsertableTitleTypeTableModel::getTableString);
		new TitleRowHtml(this).select(InsertableTitleTypeTableModel::getTitleRowModel);
		new InsertRowHtml(this).select(InsertableTitleTypeTableModel::getInsertRowModel);
		new InstanceRowHtml(this).forEach(InsertableTitleTypeTableModel::getSubModels);
	}

}
