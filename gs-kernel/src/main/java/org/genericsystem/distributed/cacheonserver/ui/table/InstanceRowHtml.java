package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class InstanceRowHtml<M extends InstanceRowModel> extends HtmlSection<M> {

	public InstanceRowHtml(TypeTableHtml<?> parent) {
		super(parent);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<InstanceRowModel>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(InstanceRowModel::getString);
		new InstanceAttributeCellHtml<CompositeModel<GenericModel>>(this).forEach(InstanceRowModel::getSubModels);
		new HtmlButton<InstanceRowModel>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(InstanceRowModel::remove).setText("Remove");
	}
}
