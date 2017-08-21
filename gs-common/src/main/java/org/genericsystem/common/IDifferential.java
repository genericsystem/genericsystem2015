package org.genericsystem.common;

import java.util.Map;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.defaults.DefaultGeneric;

import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public interface IDifferential<T extends DefaultGeneric<T>> {

	long getTs();

	Snapshot<T> getDependencies(T generic);

	ObjectProperty<IDifferential<T>> getDifferentialProperty();

	void apply(Snapshot<T> removes, Snapshot<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	public ObservableValue<?> getObservable(Generic generic);

	Map<Generic, ObservableList<Generic>> getDependenciesAsOservableListCacheMap();

	// CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

}
