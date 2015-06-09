package org.genericsystem.cache;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

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
		serverTransaction.applyFromExternal(() -> removes.stream().map(remove -> remove.getTs()), () -> adds.stream().map(add -> getRoot().getVertex(add.getTs())));
	}

	private final Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic ancestor) {
		Snapshot<Generic> dependencies = dependenciesMap.get(ancestor);
		if (dependencies == null) {
			org.genericsystem.kernel.Generic serverGeneric = serverTransaction.getGenericFromTs(ancestor.getTs());
			if (serverGeneric != null)
				dependencies = () -> serverTransaction.getDependencies(serverGeneric).stream().map(serverDependency -> serverDependency.getTs()).map(ts -> getRoot().getGenericFromTs(ts));
			else
				dependencies = () -> Stream.empty();
			Snapshot<Generic> result = dependenciesMap.put(ancestor, dependencies);
			assert result == null;
		}
		return dependencies;
	}
}
