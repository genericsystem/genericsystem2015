package org.genericsystem.distributed.ui.models;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.models.CompositeModel.Builder;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class CompositeHtmlSection<M extends CompositeModel<?>> extends GenericHtmlSection<M> {

	private final CompositeGenericConstructor<M, ?> modelConstructor;

	public CompositeHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor, CompositeGenericConstructor<M, ?> modelConstructor) {
		super(parent, stringExtractor);
		this.modelConstructor = modelConstructor;
	}

	public CompositeGenericConstructor<M, ?> getModelConstructor() {
		return modelConstructor;
	}

	public CompositeModel<?> build(Generic... generics) {
		return makeModelBuilder().apply(generics);
	}

	@Override
	protected Builder<M> makeModelBuilder() {
		ObservableList<CompositeModel<?>> subModels = FXCollections.observableArrayList();
		return generics -> {
			for (Element<?, ?> elt : getChildren()) {
				final Builder<?> leafBuilder = ((GenericHtmlSection) elt).makeModelBuilder();
				subModels.add(leafBuilder.apply(generics));
			}
			return getModelConstructor().build(generics, getStringExtractor(), subModels);
		};
	}
}