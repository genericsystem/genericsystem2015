package org.genericsystem.cache;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.ITransaction;
import org.genericsystem.common.Vertex;

public class ClientTransaction implements ITransaction<Generic> {

	private final Engine engine;
	private final long ts;

	// private final org.genericsystem.kernel.Transaction serverTransaction;

	protected ClientTransaction(Engine engine, long ts) {
		this.engine = engine;
		this.ts = ts;
		// this.serverTransaction = new org.genericsystem.kernel.Transaction(engine.getServer(), ts);
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
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert adds.stream().allMatch(add -> add.getOtherTs()[0] == Long.MAX_VALUE);
		engine.getServer().apply(getTs(), removes.stream().mapToLong(g -> g.getTs()).toArray(), adds.stream().map(g -> g.getVertex()).toArray(Vertex[]::new));
		removes.forEach(remove -> dependenciesMap.remove(remove));
		adds.forEach(add -> dependenciesMap.remove(add));
		adds.forEach(add -> add.getOtherTs()[0] = getTs());
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = () -> Arrays.stream(engine.getServer().getDependencies(getTs(), generic.getTs())).mapToObj(ts -> getRoot().getGenericById(ts));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}
}
