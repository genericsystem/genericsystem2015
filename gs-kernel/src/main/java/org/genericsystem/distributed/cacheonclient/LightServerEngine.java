package org.genericsystem.distributed.cacheonclient;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;
import org.genericsystem.kernel.Transaction;

public class LightServerEngine extends Root implements Generic, ClientCacheProtocole {

	public LightServerEngine() {
		this(new Class[] {});
	}

	public LightServerEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public LightServerEngine(String value, Class<?>... userClasses) {
		this(value, null, userClasses);
	}

	public LightServerEngine(String value, String persistentDirectoryPath, Class<?>... userClasses) {
		super(value, persistentDirectoryPath, userClasses);
	}

	@Override
	public Vertex[] getDependencies(long ts, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().stream(ts).map(generic -> generic.getVertex()).toArray(Vertex[]::new) : Statics.EMPTY;
	}

	@Override
	public void apply(long ts, long[] removeIds, Vertex[] addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert Arrays.stream(addVertices).allMatch(addVertex -> getRoot().getGenericById(addVertex.getTs()) == null);
		assert Arrays.stream(addVertices).allMatch(addVertex -> addVertex.getBirthTs() == Long.MAX_VALUE);
		Snapshot<Generic> removes = new Container(Arrays.stream(removeIds).mapToObj(removeId -> getRoot().getGenericById(removeId)));
		Snapshot<Generic> adds = new Container(Arrays.stream(addVertices).map(addVertex -> getRoot().build(addVertex)));
		try {
			new Transaction(this, ts).apply(removes, adds);
		} catch (ConcurrencyControlException | OptimisticLockConstraintViolationException e) {
			adds.forEach(add -> getRoot().release(add.getTs()));
			throw e;
		}
	}

	@Override
	public LightServerEngine getRoot() {
		return (LightServerEngine) super.getRoot();
	}
}
