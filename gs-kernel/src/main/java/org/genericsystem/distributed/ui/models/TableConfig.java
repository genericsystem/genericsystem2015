package org.genericsystem.distributed.ui.models;

import java.util.ArrayList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel.Builder;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;
import org.genericsystem.distributed.ui.models.TableConfig.Step;

public class TableConfig extends ArrayList<Step> {

	private static final long serialVersionUID = 3725849987775049853L;

	public void pushStep(ObservableListExtractor observableListExtractor) {
		pushStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, CompositeModel::new);
	}

	public void pushStep(ObservableListExtractor observableListExtractor, CompositeConstructor modelConstructor) {
		pushStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, modelConstructor);
	}

	public void pushStep_(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		pushStep(stringExtractor, observableListExtractor, CompositeModel::new);
	}

	public void pushStep(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, CompositeConstructor modelConstructor) {
		super.add(0, new Step(stringExtractor, observableListExtractor, modelConstructor));
	}

	public void pushSimpleStep() {
		pushSimpleStep(GenericModel.SIMPLE_CLASS_EXTRACTOR);
	}

	public void pushSimpleStep(StringExtractor stringExtractor) {
		super.add(0, new Step(stringExtractor, null, GenericModel::new));
	}

	public void pushSimpleStep(StringExtractor stringExtractor, CompositeConstructor modelConstructor) {
		super.add(0, new Step(stringExtractor, null, modelConstructor));
	}

	public <T extends CompositeModel<?>> T build(Generic... generics) {
		return (T) getBuilder().apply(generics);
	}

	private Builder<?> getBuilder() {
		Builder<?> builder = null;
		for (Step step : this) {
			final Builder<?> leafBuilder = builder;
			builder = generics -> step.build(generics, leafBuilder);
		}
		return builder;
	}

	@FunctionalInterface
	public interface CompositeConstructor<T extends CompositeModel<?>> {
		T build(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder);
	}

	public static class Step {

		private final StringExtractor stringExtractor;
		private final ObservableListExtractor observableListExtractor;
		private final CompositeConstructor modelConstructor;

		public Step(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, CompositeConstructor modelConstructor) {
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.modelConstructor = modelConstructor;
		}

		public CompositeModel<?> build(Generic[] generics, Builder<?> leafBuilder) {
			return modelConstructor.build(generics, stringExtractor, observableListExtractor, leafBuilder);
		}
	}
}