package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.cacheonserver.ui.list.GSCompositeHtml;
import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

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
