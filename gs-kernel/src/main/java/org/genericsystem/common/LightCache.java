package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.distributed.LightClientEngine;

public abstract class LightCache extends AbstractCache implements DefaultCache<Generic> {

	private IDifferential<Generic> transaction;
	protected TransactionDifferential differential;

	public void shiftTs() throws RollbackException {
		transaction = buildTransaction();
	}

	protected abstract IDifferential<Generic> buildTransaction();

	protected LightCache(LightClientEngine root) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	public IDifferential<Generic> getTransaction() {
		return transaction;
	}

	protected LightCache(LightClientEngine root, ContextEventListener<Generic> listener) {
		super(root);
		this.transaction = buildTransaction();
		initialize();
	}

	@Override
	public LightClientEngine getRoot() {
		return (LightClientEngine) super.getRoot();
	}

	@Override
	public long getTs() {
		return transaction.getTs();
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic vertex) {
		return differential.getDependencies(vertex);
	}

	protected void initialize() {
		differential = new TransactionDifferential();
	}

	@Override
	public void tryFlush() throws ConcurrencyControlException {
		long result = getRoot().getServer().tryFlush();
		if(result==-2)
			rollback
	}

	@Override
	public void flush() {
		long result = getRoot().getServer().flush();
		KK

	public void clear() {
		getRoot().getServer().clear();
		initialize();
	}

	public void mount() {
		getRoot().getServer().mount();
		differential = new Differential(differential);
	}

	public void unmount() {
		getRoot().getServer().unmount();
		IDifferential<Generic> subCache = differential.getSubCache();
		differential = subCache instanceof Differential ? (Differential) subCache : new Differential(subCache);
	}

	protected void checkConstraints() throws RollbackException {
		differential.checkConstraints(getChecker());
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		clear();
		throw new RollbackException(exception);
	}

	public int getCacheLevel() {
		return getRoot().getServer().getCacheLevel();
		//return differential.getCacheLevel();
	}

	@Override
	public Generic setInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericById(
				getRoot().getServer().setInstance(meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
	}

	@Override
	public Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().addInstance(meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().update(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericById(
				getRoot().getServer().merge(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public void forceRemove(Generic generic) {
		getRoot().getServer().forceRemove(generic.getTs());
		// getRestructurator().rebuildAll(null, null, computeDependencies(generic));
	}

	@Override
	public void remove(Generic generic) {
		getRoot().getServer().remove(generic.getTs());
		// getRestructurator().rebuildAll(null, null, computeRemoveDependencies(generic));
	}

	@Override
	public void conserveRemove(Generic generic) {
		getRoot().getServer().conserveRemove(generic.getTs());
		// getRestructurator().rebuildAll(generic, () -> generic, computeDependencies(generic));
	}

	private class TransactionDifferential {

		public Snapshot<Generic> getDependencies(Generic vertex) {
			return transaction.getDependencies(vertex);
		}

		public long getTs() {
			return transaction.getTs();
		}
	}
}
