package org.genericsystem.distributed.ui.models;

import java.util.ArrayList;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.cacheonserver.ui.table.InstanceRowModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericCompositeModel<M extends Model> extends Model {

	private final CompositeConstructor<M> compositeConstructor;
	private final CompositeConf<M> compositeConf;
	private final ObservableList<M> subModels;

	private Generic[] getGenerics() {
		return compositeConf.getGenerics();
	}

	private Generic[] addToGenerics(Generic generic) {
		Generic[] result = new Generic[getGenerics().length + 1];
		result[0] = generic;
		System.arraycopy(getGenerics(), 0, result, 1, getGenerics().length);
		// System.out.println(Arrays.toString(result));
		return result;
	}

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	public GenericCompositeModel(CompositeBuilder<M> compositeConstructor) {

	}

	public GenericCompositeModel(CompositeConf<M> compositeConf) {
		this.compositeConf = compositeConf;
		this.subModels = compositeConf.getObservableListExtractor() != null ? new Transformation2<>(compositeConf.getObservableListExtractor().apply(getGenerics()), generic -> (M) compositeConf.getBuilder().apply(addToGenerics(generic))) : null;
	}

	public Generic getGeneric() {
		return compositeConf.getGenerics()[0];
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(compositeConf.getString());
	}

	@FunctionalInterface
	public static interface CompositeBuilder<T> {
		T build(Generic[] generics, Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<Generic[], ?> builder);
	}

	public static class CompositeConf<M extends Model> {
		private final Generic[] generics;
		private final Function<Generic, String> stringExtractor;
		private final Function<Generic[], ObservableList<Generic>> observableListExtractor;
		private final Function<Generic[], ?> builder;

		public CompositeConf(Generic[] generics, Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<Generic[], ?> builder) {
			assert stringExtractor != null;
			this.generics = generics;
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.builder = builder;
		}

		public Generic[] getGenerics() {
			return generics;
		}

		public Function<Generic, String> getStringExtractor() {
			return stringExtractor;
		}

		public Function<Generic[], ObservableList<Generic>> getObservableListExtractor() {
			return observableListExtractor;
		}

		public Function<Generic[], ?> getBuilder() {
			return builder;
		}

		public String getString() {
			return getStringExtractor().apply(getGenerics()[0]);
		}
	}

	public static <T extends GenericCompositeModel<?>> T buildCompositeModel(Generic generic, Function<Generic[], ObservableList<Generic>> attributesExtractor) {
		MetaConfig confs = new MetaConfig();
		confs.addStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, null, GenericModel::new);
		confs.addStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[1].getObservableHolders(generics[0]), GenericCompositeModel::new);
		confs.addStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, attributesExtractor, InstanceRowModel::new);
		confs.addStep(GenericModel.SIMPLE_CLASS_EXTRACTOR, generics -> generics[0].getObservableSubInstances(), TypeTableModel::new);
		return (T) confs.getBuilder().apply(new Generic[] { generic });
	}

	public static class MetaConfig extends ArrayList<Step> {

		private static final long serialVersionUID = 3725849987775049853L;

		public void addStep(Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<CompositeConf, GenericCompositeModel<?>> modelConstructor) {
			super.add(new Step(stringExtractor, observableListExtractor, modelConstructor));
		}

		public Function<Generic[], GenericCompositeModel<?>> getBuilder() {
			Function<Generic[], GenericCompositeModel<?>> builder = null;
			for (Step conf : this) {
				final Function<Generic[], GenericCompositeModel<?>> leafBuilder = builder;
				builder = generics -> conf.build(generics, leafBuilder);
			}
			return builder;
		}
	}

	public static class Step {

		private final Function<Generic, String> stringExtractor;
		private final Function<Generic[], ObservableList<Generic>> observableListExtractor;
		private final Function<CompositeConf, GenericCompositeModel<?>> modelConstructor;

		public Step(Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<CompositeConf, GenericCompositeModel<?>> modelConstructor) {
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.modelConstructor = modelConstructor;
		}

		public Function<Generic, String> getStringExtractor() {
			return stringExtractor;
		}

		public Function<Generic[], ObservableList<Generic>> getObservableListExtractor() {
			return observableListExtractor;
		}

		public Function<CompositeConf, GenericCompositeModel<?>> getModelConstructor() {
			return modelConstructor;
		}

		public GenericCompositeModel<?> build(Generic[] generics, Function<Generic[], GenericCompositeModel<?>> leafBuilder) {
			return getModelConstructor().apply(new CompositeConf<>(generics, getStringExtractor(), getObservableListExtractor(), leafBuilder));
		}
	}
}
