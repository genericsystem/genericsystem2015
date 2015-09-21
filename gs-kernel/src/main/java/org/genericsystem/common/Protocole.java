package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public interface Protocole {

	long pickNewTs();

	Vertex getVertex(long id);

	void close();

	public static interface ClientCacheProtocole extends Protocole {

		Vertex[] getDependencies(long ts, long id);

		void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	}

	public static interface ServerCacheProtocole extends Protocole {

		Vertex[] getDependencies(long cacheId, long id);

		Vertex addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components);

		Vertex update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components);

		long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components);

		long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components);

		long forceRemove(long cacheId, long generic);

		long remove(long cacheId, long generic);

		long conserveRemove(long cacheId, long generic);

		long flush(long cacheId);

		long tryFlush(long cacheId);

		long clear(long cacheId);

		long mount(long cacheId);

		long unmount(long cacheId);

		int getCacheLevel(long cacheId);

		long shiftTs(long cacheId);

	}

}
