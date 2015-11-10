package org.genericsystem.distributed.cacheonserver;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.distributed.cacheonserver.LightClientEngine.ClientEngineHandler;
import org.genericsystem.kernel.Statics;

public class LightClientCache extends AbstractCache implements DefaultCache<Generic> {

	private final long cacheId;
	private LightClientTransaction transaction;

	public void shiftTs() throws RollbackException {

		long birthTs = getRoot().getServer().shiftTs(cacheId);
		this.transaction = new LightClientTransaction(getRoot(), cacheId);

		Iterator<Map.Entry<Long, Generic>> iterator = getRoot().genericsByIdIterator();
		Map.Entry<Long, Generic> entryGeneric;

		while (iterator.hasNext()) {
			entryGeneric = iterator.next();
			if (!entryGeneric.getValue().isAlive())
				iterator.remove();
			else if (entryGeneric.getValue().getBirthTs() == Long.MAX_VALUE)
				((ClientEngineHandler) entryGeneric.getValue().getProxyHandler()).birthTs = birthTs;
		}
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
		long birthTs = getRoot().getServer().tryFlush(cacheId);

		Iterator<Map.Entry<Long, Generic>> iterator = getRoot().genericsByIdIterator();
		Map.Entry<Long, Generic> entryGeneric;

		while (iterator.hasNext()) {
			entryGeneric = iterator.next();
			if (entryGeneric.getValue().getBirthTs() == Long.MAX_VALUE)
				((ClientEngineHandler) entryGeneric.getValue().getProxyHandler()).birthTs = birthTs;
		}
	}

	@Override
	public void flush() {
		Throwable cause = null;
		for (int attempt = 0; attempt < Statics.ATTEMPTS; attempt++) {
			try {
				tryFlush();
				return;
			} catch (ConcurrencyControlException e) {
				cause = e;
				shiftTs();
			}
		}
		discardWithException(cause);
	}

	public void clear() {
		getRoot().getServer().clear(cacheId);
		transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	public void mount() {
		getRoot().getServer().mount(cacheId);
		// transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	public void unmount() {
		getRoot().getServer().unmount(cacheId);
		transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		try {
			clear();
		} catch (Exception e) {
			throw new RollbackException(exception);
		}
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
		transaction.forceRemove(generic, computeDependencies(generic));
	}

	@Override
	public void remove(Generic generic) {
		transaction.remove(generic, computeDependencies(generic));
		getRoot().
		// transaction = new LightClientTransaction(getRoot(), cacheId);
	}

	@Override
	public void conserveRemove(Generic generic) {
		transaction.conserveRemove(generic, computeDependencies(generic));
	}
}
