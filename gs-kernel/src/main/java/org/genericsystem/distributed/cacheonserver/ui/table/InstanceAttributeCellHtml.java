package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class InstanceAttributeCellHtml<M extends GenericModel> extends HtmlSection<M> {

	public InstanceAttributeCellHtml(InstanceRowHtml<?> parent) {
		super(parent);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HolderSubCellHtml(this).forEach(CompositeModel<GenericModel>::getSubModels);
	}
}
