package org.genericsystem.reactor.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.components.HtmlLabel;
import org.genericsystem.reactor.components.HtmlSection;
import org.genericsystem.reactor.list.GSCompositeHtml;

public class InstanceAttributeCellHtml<M extends CompositeModel> extends GSCompositeHtml<M> {

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
