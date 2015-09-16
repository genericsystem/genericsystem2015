package org.genericsystem.distributed;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;

public abstract class LightCache extends AbstractCache implements DefaultCache<Generic> {

	private LightClientTransaction transaction;
	protected TransactionDifferential differential;

	public void shiftTs() throws RollbackException {
		transaction = buildTransaction();
	}

	protected abstract LightClientTransaction buildTransaction();

	public LightClientTransaction getTransaction() {
		return transaction;
	}

	protected LightCache(LightClientEngine root) {
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
		if(result==-2)
			rollback
	}

	public void clear() {
		getRoot().getServer().clear();
		initialize();
	}

	public void mount() {
		getRoot().getServer().mount();
		cleanDependencies();
	}

	public void unmount() {
		getRoot().getServer().unmount();
		cleanDependencies();
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		clear();
		throw new RollbackException(exception);
	}

	public int getCacheLevel() {
		return getRoot().getServer().getCacheLevel();
		// return differential.getCacheLevel();
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
