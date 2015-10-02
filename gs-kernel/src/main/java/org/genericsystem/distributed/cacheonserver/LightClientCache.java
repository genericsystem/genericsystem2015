package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.List;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.kernel.Statics;

public class LightClientCache extends AbstractCache implements DefaultCache<Generic> {

	private final long cacheId;
	private LightClientTransaction transaction;

	public void shiftTs() throws RollbackException {
		getRoot().getServer().shiftTs(cacheId);
	}

	public LightClientTransaction getTransaction() {
		return transaction;
	}

	protected LightClientCache(LightClientEngine root) {
		super(root);
		cacheId = getRoot().getServer().newCacheId();
		System.out.println("Create LightCache : " + cacheId);
		this.transaction = new LightClientTransaction(root, cacheId);
	}

	@Override
	public LightClientEngine getRoot() {
		return (LightClientEngine) super.getRoot();
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic vertex) {
		return transaction.getDependencies(vertex);
	}

	@Override
	public void tryFlush() throws ConcurrencyControlException {
		long result = getRoot().getServer().tryFlush(cacheId);
		if (Statics.CONCURRENCY_CONTROL_EXCEPTION == result)
			throw new ConcurrencyControlException("");
		if (Statics.OTHER_EXCEPTION == result)
			discardWithException(new IllegalStateException("Other exception"));
	}

	@Override
	public void flush() {
		long result = getRoot().getServer().flush(cacheId);
		if (Statics.CONCURRENCY_CONTROL_EXCEPTION == result) {
			System.out.println("Change Client Transaction");
			transaction = new LightClientTransaction(getRoot(), cacheId);
		}
		if (Statics.ROLLBACK_EXCEPTION == result)
			discardWithException(new IllegalStateException("Rollback"));
		if (Statics.OTHER_EXCEPTION == result)
			discardWithException(new IllegalStateException("Other exception"));
	}

	public void clear() {
		long result = getRoot().getServer().clear(cacheId);
		transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	public void mount() {
		long result = getRoot().getServer().mount(cacheId);
		// transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	public void unmount() {
		long result = getRoot().getServer().unmount(cacheId);
		transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		clear();
		throw new RollbackException(exception);
	}

	public int getCacheLevel() {
		return transaction.getCacheLevel();
	}

	@Override
	public Generic setInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return transaction.setInstance(meta, overrides, value, components);
	}

	@Override
	public Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return transaction.addInstance(meta, overrides, value, components);
	}

	@Override
	public Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return transaction.update(update, overrides, value, components);
	}

	@Override
	public Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return transaction.merge(update, overrides, value, components);
	}

	@Override
	public void forceRemove(Generic generic) {
		transaction.forceRemove(generic);
	}

	@Override
	public void remove(Generic generic) {
		transaction.remove(generic);
	}

	@Override
	public void conserveRemove(Generic generic) {
		transaction.remove(generic);
	}
}
