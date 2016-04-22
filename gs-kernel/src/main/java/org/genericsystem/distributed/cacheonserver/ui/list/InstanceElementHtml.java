package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.GenericModel;

public class InstanceElementHtml<M extends GenericModel> extends HtmlSection<M> {

	public InstanceElementHtml(TypeListHtml<?> parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<M>(new HtmlSection<M>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(GenericModel::getString);
	}
}
