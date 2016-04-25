package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.StringModel;

public class InstanceSectionHtml<M extends StringModel> extends HtmlSection<M> {

	public InstanceSectionHtml(TypeSectionHtml<?, M> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<M>(new HtmlSection<M>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(StringModel::getString);
	}
}
