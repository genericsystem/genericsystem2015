package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
import org.genericsystem.reactor.html.HtmlLabel;

public abstract class InstanceAttributeCellHtmlTemplate<M extends CompositeModel, COMPONENT extends InstanceAttributeCellHtmlTemplate<M, COMPONENT>> extends
		CompositeSectionHtmlTemplate<M, COMPONENT> {

	public InstanceAttributeCellHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
		// addStyleClass("gscell");
		this.addStyle("display", "flex").addStyle("flex", "1").addStyle("flex-direction", "column").addStyle("margin-bottom", "3px")
				.addStyle("margin-right", "1px").addStyle("color", "#ffffff").addStyle("padding", "2px").addStyle("background-color", "#ffc0cb");
		setObservableListExtractor(ObservableListExtractor.HOLDERS);

	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new HtmlLabel<CompositeModel>(parentSection).bindText(CompositeModel::getString);
	}

	public static class InstanceAttributeCellHtml<M extends CompositeModel> extends InstanceAttributeCellHtmlTemplate<M, InstanceAttributeCellHtml<M>> {
		public InstanceAttributeCellHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}
}
