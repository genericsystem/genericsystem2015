package org.genericsystem.distributed.ui.models;

import java.util.function.Function;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericCompositeModel<M extends Model> extends Model {

	private final CompositeConf<M> compositeConf;
	private ObservableList<M> subModels;

	private Generic[] getGenerics() {

		return null;
	}

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	public GenericCompositeModel(CompositeConf<M> compositeConf) {
		this.compositeConf = compositeConf;
	}

	@Override
	public void afterParentConstruct() {
		this.subModels = new Transformation2<>(compositeConf.getObservableListExtractor().apply(getGenerics()), g -> (M) compositeConf.getBuilder().apply(compositeConf.getGeneric()));
	}

	protected Generic getGeneric() {
		return compositeConf.getGeneric();
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(compositeConf.getString());
	}

	public static class CompositeConf<M extends Model> {
		private final Generic generic;
		private final Function<Generic, String> stringExtractor;
		private final Function<Generic[], ObservableList<Generic>> observableListExtractor;
		private final Function<Generic, ?> builder;

		public CompositeConf(Generic generic, Function<Generic, String> stringExtractor, Function<Generic[], ObservableList<Generic>> observableListExtractor, Function<Generic, ?> builder) {
			this.generic = generic;
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.builder = builder;
		}

		public Generic getGeneric() {
			return generic;
		}

		public Function<Generic, String> getStringExtractor() {
			return stringExtractor;
		}

		public Function<Generic[], ObservableList<Generic>> getObservableListExtractor() {
			return observableListExtractor;
		}

		public Function<Generic, ?> getBuilder() {
			return builder;
		}

		public String getString() {
			return getStringExtractor().apply(getGeneric());
		}
	}
}
