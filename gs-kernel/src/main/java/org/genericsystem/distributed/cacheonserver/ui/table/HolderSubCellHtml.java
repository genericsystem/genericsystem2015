package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;
import org.genericsystem.distributed.ui.models.GenericHtmlSection;

public class HolderSubCellHtml extends GenericHtmlSection<CompositeModel> {

	public HolderSubCellHtml(InstanceAttributeCellHtml<?> parent, StringExtractor stringExtractor) {
		super(parent, stringExtractor);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
	}
}
