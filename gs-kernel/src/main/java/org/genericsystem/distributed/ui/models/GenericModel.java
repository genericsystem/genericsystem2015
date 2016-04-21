package org.genericsystem.distributed.ui.models;

import java.util.Objects;
import java.util.function.Function;
import javafx.beans.property.ReadOnlyStringWrapper;
import org.genericsystem.common.Generic;

public class GenericModel extends StringModel {

	private final Generic generic;

	public GenericModel(Generic generic) {
		this(generic, g -> Objects.toString(g.getValue()));
	}

	public GenericModel(Generic generic, Function<Generic, String> stringExtractor) {
		super(new ReadOnlyStringWrapper(stringExtractor.apply(generic)));
		this.generic = generic;
	}

	public Generic getGeneric() {
		return generic;
	}

}
