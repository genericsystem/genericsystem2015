package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.table.InstanceAttributeCellHtmlTemplate.InstanceAttributeCellHtml;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlLabel;

public abstract class InstanceRowHtmlTemplate<M extends CompositeModel, COMPONENT extends InstanceRowHtmlTemplate<M, COMPONENT>> extends
		CompositeSectionHtmlTemplate<M, COMPONENT> {

	public InstanceRowHtmlTemplate(HtmlSection<CompositeModel> parent) {
		super(parent);
		this.addStyle("flex-direction", "row");
		setObservableListExtractor(generics -> getAttributesExtractor().apply(generics));
	}

	@Override
	protected void initChildren() {
		new HtmlLabel<M>(new HtmlSection<>(this).addStyle("display", "flex").addStyle("flex", "1").addStyle("min-width", "200px")
				.addStyle("background-color", "#e4788b").addStyle("margin-right", "1px").addStyle("color", "#ffffff").addStyle("padding", "2px"))
				.bindText(CompositeModel::getString);
		super.initChildren();
		new HtmlButton<M>(new HtmlSection<>(this).addStyle("display", "flex").addStyle("flex", "0").addStyle("min-width", "80px")
				.addStyle("background-color", "#b4868e").addStyle("flex-direction", "column").addStyle("color", "#ffffff").addStyle("padding", "2px"))
				.bindAction(CompositeModel::remove).setText("Remove");
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
