package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

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
