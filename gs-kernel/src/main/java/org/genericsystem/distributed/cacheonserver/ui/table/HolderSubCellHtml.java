package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class HolderSubCellHtml extends HtmlSection<Model> {

	public HolderSubCellHtml(HtmlSection<?> parent) {
		super(parent);
		addStyleClass("subcell");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
	}

}
