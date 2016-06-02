package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSectionTemplate;

public class HolderSubCellHtmlTemplate<M extends CompositeModel, COMPONENT extends HolderSubCellHtmlTemplate<M, COMPONENT>> extends
		HtmlSectionTemplate<M, COMPONENT> {

	public HolderSubCellHtmlTemplate(HtmlSection<?> parent) {
		super(parent);
		this.addStyle("flex-direction", "row");

	}

	@Override
	protected void initChildren() {
		new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
	}

	public static class HolderSubCellHtml<M extends CompositeModel> extends HolderSubCellHtmlTemplate<M, HolderSubCellHtml<M>> {

		public HolderSubCellHtml(org.genericsystem.reactor.html.HtmlSectionTemplate.HtmlSection<?> parent) {
			super(parent);
		}

	}

}
