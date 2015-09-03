package org.genericsystem.cache;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.cache.ClientEngine.ClientEngineHandler;
import org.genericsystem.common.Container;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Generic;

public class ClientTransaction implements IDifferential<Generic> {

	private final ClientEngine engine;
	private final long ts;

	protected ClientTransaction(ClientEngine engine, long ts) {
		this.engine = engine;
		this.ts = ts;
	}

	protected ClientTransaction(ClientEngine engine) {
		this(engine, engine.pickNewTs());
	}

	@Override
	public long getTs() {
		return ts;
	}

	// @Override
	// protected Checker<Generic> buildChecker() {
	// return new Checker<Generic>(Transaction.this) {
	// @Override
	// public void checkAfterBuild(boolean isOnAdd, boolean isFlushTime, Generic
	// vertex) throws RollbackException {
	// checkSystemConstraintsAfterBuild(isOnAdd, isFlushTime, vertex);// Check
	// only system constraints on transactions
	// }
	// };
	// }

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert adds.stream().allMatch(add -> add.getBirthTs() == Long.MAX_VALUE);
		engine.getServer().apply(getTs(), removes.stream().mapToLong(g -> g.getTs()).toArray(), adds.stream().map(g -> g.getVertex()).toArray(Vertex[]::new));
		removes.forEach(remove -> remove.getComponents().forEach(component -> dependenciesMap.remove(component)));
		removes.forEach(remove -> remove.getComponents().forEach(superG -> dependenciesMap.remove(superG)));
		removes.forEach(remove -> dependenciesMap.remove(remove.getMeta()));
		removes.forEach(remove -> dependenciesMap.remove(remove));
		adds.forEach(add -> add.getComponents().forEach(component -> dependenciesMap.remove(component)));
		adds.forEach(add -> add.getComponents().forEach(superG -> dependenciesMap.remove(superG)));
		adds.forEach(add -> dependenciesMap.remove(add.getMeta()));
		adds.forEach(add -> dependenciesMap.remove(add));
		assert adds.stream().allMatch(add -> add.getBirthTs() == Long.MAX_VALUE);
		adds.forEach(add -> ((ClientEngineHandler) add.getProxyHandler()).birthTs = getTs());
	}

	// @Override
	ClientEngine getRoot() {
		return engine;
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = new Container(Arrays.stream(engine.getServer().getDependencies(getTs(), generic.getTs())).mapToObj(ts -> getRoot().getGenericById(ts)));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

}
