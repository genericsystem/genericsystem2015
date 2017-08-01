package org.genericsystem.common;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.defaults.DefaultGeneric;

import javafx.beans.Observable;
import javafx.beans.property.ObjectProperty;

/**
 * @author Nicolas Feybesse
 *
 */
public interface IDifferential<T extends DefaultGeneric<T>> {

	long getTs();

	Snapshot<T> getDependencies(T generic);

	ObjectProperty<IDifferential<T>> getDifferentialProperty();

	void apply(Snapshot<T> removes, Snapshot<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	public Observable getObservable(Generic generic);

	// CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

}
