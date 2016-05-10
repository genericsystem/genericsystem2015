package org.genericsystem.reactor.composite.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtml.TitleCompositeSectionHtml;
import org.genericsystem.reactor.html.HtmlSection;

public class TypeTableHtml<M extends CompositeModel> extends TitleCompositeSectionHtml<M> {

	private ObservableListExtractor attributesExtractor = ObservableListExtractor.ATTRIBUTES;

	public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	public ObservableListExtractor getAttributesExtractor() {
		return attributesExtractor;
	}

	public TypeTableHtml<M> setAttributesExtractor(ObservableListExtractor attributesExtractor) {
		this.attributesExtractor = attributesExtractor;
		return this;
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new InstanceRowHtml<>(parentSection);
	}

}
