package org.genericsystem.reactor.model;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericModel extends Model {

	protected static Logger log = LoggerFactory.getLogger(GenericModel.class);
	private final Generic[] generics;

	public GenericModel(Model parent, Generic[] generics) {
		this.parent = parent;
		this.generics = generics;
	}

	public Generic[] getGenerics() {
		return generics;
	}

	public static Generic[] addToGenerics(Generic generic, Generic[] generics) {
		if (generics.length != 0 && generics[0].equals(generic))
			return generics;
		Generic[] result = new Generic[generics.length + 1];
		result[0] = generic;
		System.arraycopy(generics, 0, result, 1, generics.length);
		return result;
	}

	public Generic getGeneric() {
		return generics[0];
	}

	public void remove() {
		getGeneric().remove();
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}
}
