package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.cacheonserver.ui.list.GSCompositeHtml;
import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class InstanceRowHtml<M extends CompositeModel> extends GSCompositeHtml<M> {

	public InstanceRowHtml(HtmlSection<CompositeModel> parent) {
		super(parent);
		addStyleClass("gsrow");
		setObservableListExtractor(generics -> getAttributesExtractor().apply(generics));
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<M>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gstitlecell")).bindText(CompositeModel::getString);
		super.initChildren();
		new HtmlButton<M>(new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gsbuttoncell")).bindAction(CompositeModel::remove).setText("Remove");
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new InstanceAttributeCellHtml<CompositeModel>(parentSection);
	}

	ObservableListExtractor getAttributesExtractor() {
		return this.getParent().<TypeTableHtml<?>> getParent().getAttributesExtractor();
	}
}
