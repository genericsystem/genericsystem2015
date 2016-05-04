package org.genericsystem.distributed.ui.models;

import javafx.collections.ObservableList;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.models.CompositeModel.Builder;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class DynamicCompositeHtmlSection<M extends CompositeModel<?>> extends CompositeHtmlSection<M> {
	private final ObservableListExtractor observableListExtractor;

	public DynamicCompositeHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, CompositeGenericConstructor<M, ?> constructor) {
		super(parent, stringExtractor, constructor);
		this.observableListExtractor = observableListExtractor;
		assert observableListExtractor != null;
	}

	@Override
	protected Builder<M> makeModelBuilder() {
		assert getChildren().size() == 1;
		GenericHtmlSection<?> element = (GenericHtmlSection) getChildren().get(0);
		final Builder<?> leafBuilder = element.makeModelBuilder();
		return generics -> getModelConstructor().build(generics, getStringExtractor(), (ObservableList) new Transformation2<>(observableListExtractor.apply(generics), generic -> leafBuilder.apply(CompositeModel.addToGenerics(generic, generics))));
	}
}