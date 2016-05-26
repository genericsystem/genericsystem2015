package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
import org.genericsystem.reactor.composite.table.InstanceAttributeCellHtmlTemplate.InstanceAttributeCellHtml;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlLabel;

public abstract class InstanceRowHtmlTemplate<M extends CompositeModel, COMPONENT extends InstanceRowHtmlTemplate<M, COMPONENT>>
		extends CompositeSectionHtmlTemplate<M, COMPONENT> {

	public InstanceRowHtmlTemplate(HtmlSection<CompositeModel> parent) {
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
		return this.getParent().<TypeTableHtmlTemplate<?, ?>> getParent().getAttributesExtractor();
	}

	public static class InstanceRowHtml<M extends CompositeModel> extends InstanceRowHtmlTemplate<M, InstanceRowHtml<M>> {

		public InstanceRowHtml(org.genericsystem.reactor.html.HtmlSectionTemplate.HtmlSection<CompositeModel> parent) {
			super(parent);
		}
	}
}
