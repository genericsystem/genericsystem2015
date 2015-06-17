package org.genericsystem.cache;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;

public class Transaction implements IDifferential {

	private final Engine engine;
	private final long ts;
	private final org.genericsystem.kernel.Transaction serverTransaction;

	protected Transaction(Engine engine, long ts) {
		this.engine = engine;
		this.ts = ts;
		this.serverTransaction = new org.genericsystem.kernel.Transaction(engine.getServer(), ts);
	}

	protected Transaction(Engine engine) {
		this(engine, engine.pickNewTs());
	}

	Engine getRoot() {
		return engine;
	}

	long getTs() {
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
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		List<Long> removesIds = removes.stream().map(remove -> remove.getTs()).collect(Collectors.toList());
		List<Vertex> addVertices = adds.stream().map(add -> add.getVertex()).collect(Collectors.toList());
		assert addVertices.stream().allMatch(add -> add.getOtherTs()[0] == Long.MAX_VALUE);
		serverTransaction.applyFromExternal(removesIds, addVertices);
		removes.stream().forEach(remove -> dependenciesMap.remove(remove));
		adds.stream().forEach(add -> dependenciesMap.remove(add));
		adds.stream().forEach(add -> add.getVertex().getOtherTs()[0] = getTs());
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = () -> serverTransaction.getDependenciesFromExternal(generic.getTs()).stream().map(ts -> getRoot().getGenericById(ts));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

	// @Override
	// public long getBirthTs(Generic generic) {
	// return generic.getVertex().getOtherTs()[0];
	// }
}
