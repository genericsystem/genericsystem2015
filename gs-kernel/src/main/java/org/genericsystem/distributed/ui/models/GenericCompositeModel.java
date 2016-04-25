package org.genericsystem.distributed.ui.models;

import java.util.function.Function;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericCompositeModel<M extends Model> extends CompositeModel<M> {

	private final CompositeConf<M> compositeConf;

	// public GenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
	// this(generic, GenericModel.EXTRACTOR, observableListExtractor, elementBuilder);
	// }

	// public GenericCompositeModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
	// super(generic, observableListExtractor, elementBuilder);
	// this.generic = generic;
	// this.stringExtractor = stringExtractor;
	// }

	public GenericCompositeModel(CompositeConf<M> compositeConf) {
		super(compositeConf.getGeneric(), compositeConf.getObservableListExtractor(), compositeConf.getElementBuilder());
		this.compositeConf = compositeConf;
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
		private final Function<Generic, ObservableList<Generic>> observableListExtractor;
		private final Function<Generic, M> elementBuilder;

		public CompositeConf(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
			this.generic = generic;
			this.stringExtractor = GenericModel.EXTRACTOR;
			this.observableListExtractor = observableListExtractor;
			this.elementBuilder = elementBuilder;
		}

		public CompositeConf(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
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

		public Function<Generic, M> getElementBuilder() {
			return elementBuilder;
		}

		public String getString() {
			return getStringExtractor().apply(getGeneric());
		}
	}
}
