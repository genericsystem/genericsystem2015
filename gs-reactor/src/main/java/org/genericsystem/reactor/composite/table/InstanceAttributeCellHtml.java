package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtml;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public class InstanceAttributeCellHtml<M extends CompositeModel> extends CompositeSectionHtml<M> {

	public InstanceAttributeCellHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gscell");
		setObservableListExtractor(ObservableListExtractor.HOLDERS);
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new HtmlLabel<CompositeModel>(parentSection).bindText(CompositeModel::getString);
	}
}
