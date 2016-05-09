package org.genericsystem.distributed.cacheonserver.ui.table.title;

import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlStrong;

class TitleCellHtml<M extends CompositeModel> extends HtmlSection<M> {

	public TitleCellHtml(TitleRowHtml<?> parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<GenericModel>(new HtmlStrong<>(this)).bindText(GenericModel::getString);
	}
}
