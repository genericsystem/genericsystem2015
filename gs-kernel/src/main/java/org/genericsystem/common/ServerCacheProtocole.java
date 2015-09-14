package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

public interface ServerCacheProtocole {

	final static Vertex[] EMPTY = new Vertex[] {};

	Vertex getVertex(long id);

	Vertex[] getDependencies(long ts, long id);

	long addInstance(long meta, List<Long> overrides, Serializable value, List<Long> components);

	long update(long update, List<Long> overrides, Serializable value, List<Long> newComponents);

	long merge(long update, List<Long> overrides, Serializable value, List<Long> newComponents);

	long setInstance(long meta, List<Long> overrides, Serializable value, List<Long> components);

	long forceRemove(long generic);

	long remove(long generic);

	long conserveRemove(long generic);

	// void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	long flush(long ts);

	long tryFlush(long ts);

	long pickNewTs();

	void close();
}
