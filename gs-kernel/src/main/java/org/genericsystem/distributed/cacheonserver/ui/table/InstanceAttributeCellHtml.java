package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.models.CompositeHtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class InstanceAttributeCellHtml<M extends CompositeModel> extends CompositeHtmlSection<M> {

	public InstanceAttributeCellHtml(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor) {
		super(parent, stringExtractor);
		addStyleClass("gscell");
	}

	@Override
	protected void initChildren() {
		new HolderSubCellHtml(this, StringExtractor.SIMPLE_CLASS_EXTRACTOR).forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, generics -> generics[1].getObservableHolders(generics[0]), CompositeModel::new);
	}
}
