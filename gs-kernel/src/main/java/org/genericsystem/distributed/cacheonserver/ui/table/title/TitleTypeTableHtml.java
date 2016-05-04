package org.genericsystem.distributed.cacheonserver.ui.table.title;

import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class TitleTypeTableHtml<M extends TitleTypeTableModel> extends TypeTableHtml<M> {
	public TitleTypeTableHtml(HtmlElement<?, ?, ?> parent, StringExtractor extractor, CompositeGenericConstructor<M, InstanceRowModel> constructor) {
		super(parent, extractor, constructor);
	}

	@Override
	protected void initChildren() {
		new HtmlH1<M>(new HtmlSection<>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TitleTypeTableModel::getString);
		new TitleRowHtml<>(this).select(TitleTypeTableModel::getTitleRowModel);
		new InstanceRowHtml<>(this).forEach(TitleTypeTableModel::getSubModels);
	}

}
