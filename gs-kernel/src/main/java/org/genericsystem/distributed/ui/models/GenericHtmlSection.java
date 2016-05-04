package org.genericsystem.distributed.ui.models;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.Builder;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class GenericHtmlSection<M extends CompositeModel<?>> extends HtmlSection<M> {
	private final StringExtractor stringExtractor;

	public GenericHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor) {
		super(parent);
		this.stringExtractor = stringExtractor;
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	protected Builder<M> makeModelBuilder() {
		assert getChildren().size() == 0;
		return gs -> (M) new GenericModel(gs, stringExtractor);
	}

}
