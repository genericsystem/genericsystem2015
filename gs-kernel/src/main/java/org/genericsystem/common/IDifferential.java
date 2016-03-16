package org.genericsystem.common;

import java.util.concurrent.CompletableFuture;
import javafx.beans.Observable;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.defaults.DefaultVertex;

/**
 * @author Nicolas Feybesse
 *
 */
public interface IDifferential<T extends DefaultVertex<T>> {

	long getTs();

	Snapshot<T> getDependencies(T generic);

	void apply(Snapshot<T> removes, Snapshot<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	public Observable getInvalidator(Generic generic);

	CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

}
