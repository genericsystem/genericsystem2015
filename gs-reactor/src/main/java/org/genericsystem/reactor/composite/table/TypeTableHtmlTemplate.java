package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate.TitleCompositeSectionHtmlTemplate;
import org.genericsystem.reactor.composite.table.InstanceRowHtmlTemplate.InstanceRowHtml;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel, COMPONENT extends TypeTableHtmlTemplate<M, COMPONENT>> extends TitleCompositeSectionHtmlTemplate<M, COMPONENT> {

	private ObservableListExtractor attributesExtractor = ObservableListExtractor.ATTRIBUTES;

	public TypeTableHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	public ObservableListExtractor getAttributesExtractor() {
		return attributesExtractor;
	}

	public TypeTableHtmlTemplate<M, COMPONENT> setAttributesExtractor(ObservableListExtractor attributesExtractor) {
		this.attributesExtractor = attributesExtractor;
		return this;
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new InstanceRowHtml<>(parentSection);
	}

	public static class TypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M, TypeTableHtml<M>> {

		public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}

}
