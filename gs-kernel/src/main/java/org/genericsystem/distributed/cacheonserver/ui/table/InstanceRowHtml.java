package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeHtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class InstanceRowHtml<M extends InstanceRowModel> extends CompositeHtmlSection<M> {

	public InstanceRowHtml(TypeTableHtml<?> parent, StringExtractor extractor) {
		super(parent, extractor);
		addStyleClass("gsrow");
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<InstanceRowModel>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(InstanceRowModel::getString);
		new InstanceAttributeCellHtml<CompositeModel>(this, StringExtractor.SIMPLE_CLASS_EXTRACTOR).forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, generics -> getAttributesExtractor().apply(generics), CompositeModel::new);
		new HtmlButton<InstanceRowModel>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(InstanceRowModel::remove).setText("Remove");
	}

	ObservableListExtractor getAttributesExtractor() {
		return this.<TypeTableHtml<?>> getParent().getAttributesExtractor();
	}
}
