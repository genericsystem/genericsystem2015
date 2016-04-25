package org.genericsystem.distributed.ui.models;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class CompositeModel<M extends Model> extends TransformationModel<M> {

	public <G> CompositeModel(G type, Function<G, ObservableList<G>> observableListExtractor, Function<G, M> elementBuilder) {
		super(observableListExtractor.apply(type), elementBuilder);
	}
}
