package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableHtml;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class InsertTitleTypeTableHtml<M extends InsertTitleTypeTableModel> extends TitleTypeTableHtml<M> {

	public InsertTitleTypeTableHtml(HtmlElement<?, ?, ?> parent, StringExtractor extractor, CompositeGenericConstructor<M, InstanceRowModel> constructor) {
		super(parent, extractor, constructor);
	}

	@Override
	protected void initChildren() {
		new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(InsertTitleTypeTableModel::getString);
		new TitleRowHtml<>(this).select(InsertTitleTypeTableModel::getTitleRowModel);
		new InsertRowHtml<>(this).select(InsertTitleTypeTableModel::getInsertRowModel);
		new InstanceRowHtml<>(this).forEach(InsertTitleTypeTableModel::getSubModels);
	}

}
