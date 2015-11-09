package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;

import org.genericsystem.common.AbstractEngine;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.IDifferential;

public class CacheOnClient extends HeavyCache {

	protected CacheOnClient(AbstractEngine root) {
		super(root);
	}

	protected CacheOnClient(AbstractEngine root, ContextEventListener<Generic> listener) {
		super(root, listener);
	}

	@Override
	protected AsyncDifferential buildDifferential(IDifferential<Generic> subCache) {
		return new AsyncDifferential((AsyncIDifferential) subCache);
	}

	@Override
	protected TransactionDifferential buildTransactionDifferential() {
		return new AsyncTransactionDifferential();
	}

	protected class AsyncTransactionDifferential extends TransactionDifferential implements AsyncIDifferential {
		@Override
		public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
			return getTransaction().getDependenciesObservableList(generic);
		}
	}

	@Override
	protected HeavyClientTransaction buildTransaction() {
		return new HeavyClientTransaction((HeavyClientEngine) (getRoot()), getRoot().pickNewTs());
	}

	@Override
	public HeavyClientTransaction getTransaction() {
		return (HeavyClientTransaction) super.getTransaction();
	}

	@Override
	protected AsyncDifferential getDifferential() {
		return (AsyncDifferential) super.getDifferential();
	}

	public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
		return getDifferential().getDependenciesObservableList(generic);
	}
}