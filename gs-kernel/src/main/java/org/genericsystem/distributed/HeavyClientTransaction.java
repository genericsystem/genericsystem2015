package org.genericsystem.distributed;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.CheckedContext;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.HeavyClientEngine.ClientEngineHandler;

public class HeavyClientTransaction extends CheckedContext implements IDifferential<Generic> {

	private final long ts;

	protected HeavyClientTransaction(HeavyClientEngine engine, long ts) {
		super(engine);
		this.ts = ts;
	}

	protected HeavyClientTransaction(HeavyClientEngine engine) {
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
		getRoot().getServer().apply(getTs(), removes.stream().mapToLong(g -> g.getTs()).toArray(), adds.stream().map(g -> g.getVertex()).toArray(Vertex[]::new));
		removes.forEach(this::invalid);// Not efficient ! plug and unplug is better
		adds.forEach(this::invalid);// Not efficient !
		adds.forEach(this::giveBirth);
	}

	private void giveBirth(Generic generic) {
		assert Long.MAX_VALUE == generic.getBirthTs();
		((ClientEngineHandler) generic.getProxyHandler()).birthTs = getTs();
	}

	private void invalid(Generic generic) {
		generic.getComponents().forEach(component -> dependenciesMap.remove(component));
		generic.getSupers().forEach(superG -> dependenciesMap.remove(superG));
		dependenciesMap.remove(generic.getMeta());
		dependenciesMap.remove(generic);
	}

	@Override
	public HeavyClientEngine getRoot() {
		return (HeavyClientEngine) super.getRoot();
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = new Container(Arrays.stream(getRoot().getServer().getDependencies(getTs(), generic.getTs())).map(vertex -> getRoot().getGenericByVertex(vertex)));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

}
