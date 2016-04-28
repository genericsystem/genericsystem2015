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
public class CompositeModel<M extends Model> extends Model {

	private final ObservableList<M> subModels;
	private final Conf compositeConf;

	public CompositeModel(Generic type, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
		this(new Conf(type, observableListExtractor, elementBuilder));
	}

	public CompositeModel(Conf compositeConf) {
		this.compositeConf = compositeConf;
		this.subModels = (ObservableList) compositeConf.observableSubModels();
	}

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	protected Generic getGeneric() {
		return compositeConf.getGeneric();
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(compositeConf.getString());
	}

	public static class Conf {
		private final Generic generic;
		private final Function<Generic, String> stringExtractor;
		private final Function<Generic, ObservableList<Generic>> observableListExtractor;
		private final Function<Generic, ? extends Model> elementBuilder;

		public Conf(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, ? extends Model> elementBuilder) {
			this(generic, GenericModel.EXTRACTOR, observableListExtractor, elementBuilder);
		}

		public Conf(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, ? extends Model> elementBuilder) {
			this.generic = generic;
			this.stringExtractor = stringExtractor;
			this.observableListExtractor = observableListExtractor;
			this.elementBuilder = elementBuilder;
		}

		public Generic getGeneric() {
			return generic;
		}

		public Function<Generic, String> getStringExtractor() {
			return stringExtractor;
		}

		public Function<Generic, ObservableList<Generic>> getObservableListExtractor() {
			return observableListExtractor;
		}

		public Function<Generic, ? extends Model> getElementBuilder() {
			return elementBuilder;
		}

		public String getString() {
			return getStringExtractor().apply(getGeneric());
		}

		public ObservableList<?> observableSubModels() {
			return new Transformation2<>(getObservableListExtractor().apply(getGeneric()), getElementBuilder());
		}
	}
}
