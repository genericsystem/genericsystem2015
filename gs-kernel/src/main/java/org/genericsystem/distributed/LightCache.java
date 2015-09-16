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
import org.genericsystem.kernel.Statics;

public class LightCache extends AbstractCache implements DefaultCache<Generic> {

	private LightClientTransaction transaction;

	public void shiftTs() throws RollbackException {
		getRoot().getServer().shiftTs();
	}

	public LightClientTransaction getTransaction() {
		return transaction;
	}

	protected LightCache(LightClientEngine root) {
		super(root);
		this.transaction = new LightClientTransaction(root);
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
		return transaction.getDependencies(vertex);
	}

	@Override
	public void tryFlush() throws ConcurrencyControlException {
		long result = getRoot().getServer().tryFlush();
		if (Statics.CONCURRENCY_CONTROL_EXCEPTION == result)
			throw new ConcurrencyControlException("");
		if (Statics.OTHER_EXCEPTION == result)
			discardWithException(new IllegalStateException("Other exception"));
		assert result == getTs();
	}

	@Override
	public void flush() {
		long result = getRoot().getServer().flush();
		if (Statics.OTHER_EXCEPTION == result)
			discardWithException(new IllegalStateException("Other exception"));
		if (result != getTs()) {
			assert result > getTs();
			transaction = new LightClientTransaction(getRoot(), result);
		}
	}

	public void clear() {
		getRoot().getServer().clear();
		transaction.initializeDependencies();
	}

	public void mount() {
		getRoot().getServer().mount();
	}

	public void unmount() {
		getRoot().getServer().unmount();
		transaction.initializeDependencies();
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
		transaction.initializeDependencies();

	}

	@Override
	public Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().addInstance(meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		transaction.initializeDependencies();
	}

	@Override
	public Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().update(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		transaction.initializeDependencies();
	}

	@Override
	public Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericById(
				getRoot().getServer().merge(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
		transaction.initializeDependencies();
	}

	@Override
	public void forceRemove(Generic generic) {
		getRoot().getServer().forceRemove(generic.getTs());
		transaction.initializeDependencies();
	}

	@Override
	public void remove(Generic generic) {
		getRoot().getServer().remove(generic.getTs());
		transaction.initializeDependencies();
	}

	@Override
	public void conserveRemove(Generic generic) {
		getRoot().getServer().conserveRemove(generic.getTs());
		transaction.initializeDependencies();
	}
}
