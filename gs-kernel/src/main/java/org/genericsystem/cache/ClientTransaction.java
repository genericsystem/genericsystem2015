package org.genericsystem.cache;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.ITransaction;
import org.genericsystem.common.Vertex;

public class ClientTransaction implements ITransaction<Generic> {

	private final Engine engine;
	private final long ts;
	private final org.genericsystem.kernel.Transaction serverTransaction;

	protected ClientTransaction(Engine engine, long ts) {
		this.engine = engine;
		this.ts = ts;
		this.serverTransaction = new org.genericsystem.kernel.Transaction(engine.getServer(), ts);
	}

	protected ClientTransaction(Engine engine) {
		this(engine, engine.pickNewTs());
	}

	@Override
	public Engine getRoot() {
		return engine;
	}

	@Override
	public long getTs() {
		return ts;
	}

	// @Override
	// protected Checker<Generic> buildChecker() {
	// return new Checker<Generic>(Transaction.this) {
	// @Override
	// public void checkAfterBuild(boolean isOnAdd, boolean isFlushTime, Generic vertex) throws RollbackException {
	// checkSystemConstraintsAfterBuild(isOnAdd, isFlushTime, vertex);// Check only system constraints on transactions
	// }
	// };
	// }

	@Override
	public void apply(Iterable<Generic> removes, Iterable<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		List<Long> removesIds = new ArrayList<>();
		removes.forEach(remove -> removesIds.add(remove.getTs()));
		List<Vertex> addVertices = new ArrayList<>();
		adds.forEach(add -> addVertices.add(add.getVertex()));
		assert addVertices.stream().allMatch(add -> add.getOtherTs()[0] == Long.MAX_VALUE);
		serverTransaction.remoteApply(removesIds, addVertices);
		removes.forEach(remove -> dependenciesMap.remove(remove));
		adds.forEach(add -> dependenciesMap.remove(add));
		adds.forEach(add -> add.getOtherTs()[0] = getTs());
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = () -> serverTransaction.getRemoteDependencies(generic.getTs()).stream().map(ts -> getRoot().getGenericById(ts));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}
}
