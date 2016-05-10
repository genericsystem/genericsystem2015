package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtml;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public class InstanceRowHtml<M extends CompositeModel> extends CompositeSectionHtml<M> {

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
