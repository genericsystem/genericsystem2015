package org.genericsystem.distributed.cacheonclient;

import java.util.function.Consumer;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Protocole;
import org.genericsystem.common.Vertex;

public interface ClientCacheProtocole extends Protocole {

	Vertex[] getDependencies(long ts, long id);

	void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	default void sendRequestForObservableDependencies(Consumer container, HeavyClientEngine root, long ts, long id) {
	}

}