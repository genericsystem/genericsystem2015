package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Protocole;
import org.genericsystem.common.Vertex;

public interface ServerCacheProtocole extends Protocole {

	Vertex[] getDependencies(long cacheId, long id);

	long addInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components);

	long update(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components);

	long merge(long cacheId, long update, List<Long> overrides, Serializable value, List<Long> components);

	long setInstance(long cacheId, long meta, List<Long> overrides, Serializable value, List<Long> components);

	long forceRemove(long cacheId, long generic);

	long remove(long cacheId, long generic);

	long conserveRemove(long cacheId, long generic);

	long flush(long cacheId);

	long tryFlush(long cacheId) throws ConcurrencyControlException;

	long clear(long cacheId);

	long mount(long cacheId);

	long unmount(long cacheId);

	int getCacheLevel(long cacheId);

	long shiftTs(long cacheId);

	long newCacheId();

}
