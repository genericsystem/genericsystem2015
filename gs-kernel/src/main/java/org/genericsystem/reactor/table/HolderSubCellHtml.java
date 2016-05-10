package org.genericsystem.reactor.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.components.HtmlLabel;
import org.genericsystem.reactor.components.HtmlSection;

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
