package org.genericsystem.distributed.ui.models;

import java.util.ArrayList;
import java.util.List;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.models.CompositeModel.Builder;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class SimpleStep {

	private final StringExtractor stringExtractor;
	private final List<SimpleStep> children = new ArrayList<>();

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	protected SimpleStep(StringExtractor stringExtractor) {
		this.stringExtractor = stringExtractor;
	}

	public List<SimpleStep> getChildren() {
		return children;
	}

	protected Builder<?> makeModelBuilder() {
		assert getChildren().size() == 0;
		return gs -> new GenericModel(gs, stringExtractor);
	}

	public static class CompositeStep extends SimpleStep {

		private final CompositeGenericConstructor compositeconstructor;

		public CompositeStep(StringExtractor stringExtractor, CompositeGenericConstructor compositeconstructor) {
			super(stringExtractor);
			this.compositeconstructor = compositeconstructor;
		}

		public CompositeGenericConstructor getModelConstructor() {
			return compositeconstructor;
		}

		public CompositeModel<?> build(Generic... generics) {
			return makeModelBuilder().apply(generics);
		}

		@Override
		protected Builder<?> makeModelBuilder() {
			ObservableList<CompositeModel<?>> subModels = FXCollections.observableArrayList();
			return generics -> {
				for (SimpleStep step : getChildren()) {
					final Builder<?> leafBuilder = step.makeModelBuilder();
					subModels.add(leafBuilder.apply(generics));
				}
				return compositeconstructor.build(generics, getStringExtractor(), subModels);
			};
		}

		public Step addChildStep(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, CompositeGenericConstructor modelConstructor) {
			Step step = new Step(stringExtractor, observableListExtractor, modelConstructor);
			getChildren().add(step);
			return step;
		}

		public CompositeStep addChildCompositeStep(StringExtractor stringExtractor, CompositeGenericConstructor modelConstructor) {
			CompositeStep step = new CompositeStep(stringExtractor, modelConstructor);
			getChildren().add(step);
			return step;
		}

		public Step addChildStep(ObservableListExtractor observableListExtractor) {
			return addChildStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, CompositeModel::new);
		}

		public Step addChildStep(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
			return addChildStep(stringExtractor, observableListExtractor, CompositeModel::new);
		}

		public Step addChildStep(ObservableListExtractor observableListExtractor, CompositeGenericConstructor modelConstructor) {
			return addChildStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, modelConstructor);
		}

		public SimpleStep addChildSimpleStep(StringExtractor stringExtractor) {
			SimpleStep step = new SimpleStep(stringExtractor);
			getChildren().add(step);
			return step;
		}

		public SimpleStep addChildSimpleStep() {
			return addChildSimpleStep(GenericModel.SIMPLE_CLASS_EXTRACTOR);
		}

	}

	public static class Step extends CompositeStep {

		private final ObservableListExtractor observableListExtractor;

		public Step(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, CompositeGenericConstructor modelConstructor) {
			super(stringExtractor, modelConstructor);
			this.observableListExtractor = observableListExtractor;
		}

		@Override
		public CompositeModel<?> build(Generic... generics) {
			return makeModelBuilder().apply(generics);
		}

		@Override
		protected Builder<?> makeModelBuilder() {
			assert !getChildren().isEmpty();
			if (getChildren().size() != 1) {
				assert observableListExtractor == null;
			}
			SimpleStep step = getChildren().get(0);
			final Builder<?> leafBuilder = step.makeModelBuilder();
			return generics -> getModelConstructor().build(generics, getStringExtractor(), new Transformation2<>(observableListExtractor.apply(generics), generic -> leafBuilder.apply(CompositeModel.addToGenerics(generic, generics))));
		}

		public ObservableListExtractor getObservableListExtractor() {
			return observableListExtractor;
		}
	}
}