package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.IDifferential;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

public class CocCache extends HeavyCache {

	private Map<Generic, ObservableList<Generic>> dependenciesAsOservableListCacheMap = new HashMap<Generic, ObservableList<Generic>>() {

		private static final long serialVersionUID = -6268341397267444297L;

		@Override
		public ObservableList<Generic> get(Object key) {
			ObservableList<Generic> result = super.get(key);
			if (result == null) {
				ObservableValue<List<Generic>> dependenciesAsObservableValue = getDifferential().getDependenciesObservableList((Generic) key);
				result = new ListBinding<Generic>() {
					{
						bind(dependenciesAsObservableValue);
					}

					@Override
					public void dispose() {
						unbind(dependenciesAsObservableValue);
					}

					@Override
					protected ObservableList<Generic> computeValue() {
						return FXCollections.observableArrayList(dependenciesAsObservableValue.getValue());
					}
				};
				ObservableList<Generic> assertResult = super.put((Generic) key, result);
				assert assertResult == null;
			}
			return result;

		}
	};

	protected CocCache(AbstractRoot root) {
		super(root);
	}

	protected CocCache(AbstractRoot root, ContextEventListener<Generic> listener) {
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

	private ObservableValue<AsyncITransaction> transactionInvalidator;
	private ObservableValue<AsyncITransaction> differentialInvalidator;

	protected class AsyncTransactionDifferential extends TransactionDifferential implements AsyncIDifferential {
		@Override
		public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic) {
			return getTransaction().getDependenciesObservableList(generic);
		}

		@Override
		public ObservableList<Generic> getWrappableDependencies(Generic generic) {
			return getTransaction().getWrappableDependencies(generic);
		}

		@Override
		public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic) {
			return getTransaction().getDependenciesObservableSnapshot(generic);
		}

		@Override
		public ObservableValue<CompletableFuture<Snapshot<Generic>>> getDependenciesPromise(Generic generic) {
			return Bindings.<CompletableFuture<Snapshot<Generic>>> createObjectBinding(() -> getTransaction().getDependenciesPromise(generic), transactionInvalidator);
		}
	}

	@Override
	protected CocTransaction buildTransaction() {
		return new CocTransaction((CocClientEngine) (getRoot()), getRoot().pickNewTs());
	}

	@Override
	public AsyncITransaction getTransaction() {
		return (AsyncITransaction) super.getTransaction();
	}

	@Override
	protected AsyncDifferential getDifferential() {
		return (AsyncDifferential) super.getDifferential();
	}

	public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
		return dependenciesAsOservableListCacheMap.get(generic);
	}

	public ObservableList<Generic> getWrappableDependenciesSnap(Generic generic) {
		return getDifferential().getDependenciesObservableSnapshot(generic).toObservableList();
	}

	public ObservableList<Generic> getWrappableDependencies(Generic generic) {
		return getDifferential().getWrappableDependencies(generic);
	}

	public ObservableList<Generic> getInstancesObservableList(Generic meta) {
		return getDependenciesObservableList(meta).filtered(generic -> meta.equals(generic.getMeta()));
	}

	public CompletableFuture<ObservableList<Generic>> getDependenciesPromise(Generic generic) throws InterruptedException, ExecutionException {
		return getDifferential().getDependenciesPromise(generic).getValue().thenApply(snapshot -> {
			ObservableList<Generic> observableList = dependenciesPromiseAsOservableListCacheMap.get(generic);
			observableList.setAll(snapshot.toList());
			return FXCollections.unmodifiableObservableList(observableList);
		});
	}

	private Map<Generic, ObservableList<Generic>> dependenciesPromiseAsOservableListCacheMap = new HashMap<Generic, ObservableList<Generic>>() {

		private static final long serialVersionUID = -6483309004505712513L;

		@Override
		public ObservableList<Generic> get(Object key) {
			ObservableList<Generic> result = super.get(key);
			if (result == null)
				put((Generic) key, result = FXCollections.observableArrayList());
			return result;
		}
	};
}
