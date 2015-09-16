package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public interface Protocole {

	long pickNewTs();

	Vertex[] getDependencies(long ts, long id);

	Vertex getVertex(long id);

	void close();

	public static interface ClientCacheProtocole extends Protocole {

		void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	}

	public static interface ServerCacheProtocole extends Protocole {

		Vertex addInstance(long meta, List<Long> overrides, Serializable value, List<Long> components);

		Vertex update(long update, List<Long> overrides, Serializable value, List<Long> newComponents);

		long merge(long update, List<Long> overrides, Serializable value, List<Long> newComponents);

		long setInstance(long meta, List<Long> overrides, Serializable value, List<Long> components);

		long forceRemove(long generic);

		long remove(long generic);

		long conserveRemove(long generic);

		long flush();

		long tryFlush();

		void clear();

		void mount();

		void unmount();

		int getCacheLevel();

	}

}
