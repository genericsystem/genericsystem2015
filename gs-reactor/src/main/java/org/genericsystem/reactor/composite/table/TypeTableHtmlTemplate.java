package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate.TitleCompositeSectionHtmlTemplate;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlLabel;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel, COMPONENT extends TypeTableHtmlTemplate<M, COMPONENT>> extends TitleCompositeSectionHtmlTemplate<M, COMPONENT> {

	private ObservableListExtractor subObservableListExtractor = ObservableListExtractor.ATTRIBUTES;

	public TypeTableHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyle("flex-direction", "column");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	public ObservableListExtractor getSubObservableListExtractor() {
		return subObservableListExtractor;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setSubObservableListExtractor(ObservableListExtractor subObservableListExtractor) {
		this.subObservableListExtractor = subObservableListExtractor;
		return (COMPONENT) this;
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new CompositeSectionHtml<CompositeModel>(parentSection) {
			@Override
			protected void initChildren() {
				new HtmlLabel<M>(new HtmlSection<>(this).addStyle("min-width", "200px")).bindText(CompositeModel::getString);
				super.initChildren();
				new HtmlButton<M>(new HtmlSection<>(this).addStyle("min-width", "80px")).bindAction(CompositeModel::remove).setText("Remove");
			}

			@Override
			protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
				new CompositeSectionHtml<CompositeModel>(parentSection) {
					@Override
					protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
						new HtmlSection<CompositeModel>(parentSection) {
							@Override
							protected void initChildren() {
								new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
							}
						}.addStyle("flex-direction", "row");
					}
				}.addStyle("flex-direction", "column").setObservableListExtractor(ObservableListExtractor.HOLDERS);
			}
		}.setObservableListExtractor(gs -> getSubObservableListExtractor().apply(gs)).addStyle("flex-direction", "row");
	}

	public static class TypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M, TypeTableHtml<M>> {
		public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}

	// public static class ColumnTitleTypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M, ColumnTitleTypeTableHtml<M>> {
	// public ColumnTitleTypeTableHtml(HtmlElement<?, ?, ?> parent) {
	// super(parent);
	// }
	//
	// @Override
	// protected void initChildren() {
	// new HtmlH1<M>(new HtmlSection<M>(this)).bindText(CompositeModel::getString);
	// HtmlSection<CompositeModel> subSection = new HtmlSection<CompositeModel>(this);
	// subSection.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, constructor) -> getModelConstructor().build(gs, constructor));
	// initSubChildren(subSection);
	// }
	// }

}
