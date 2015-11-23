package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
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
			return Bindings.<CompletableFuture<Snapshot<Generic>>> createObjectBinding(() -> getTransaction().getDependenciesPromise(generic), transactionProperty);
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
		ObservableValue<CompletableFuture<Snapshot<Generic>>> dependenciesPromise = getDifferential().getDependenciesPromise(generic);
		return dependenciesPromise.getValue().thenApply(snapshot -> {
			GSListBinding observableList = dependenciesPromiseAsOservableListCacheMap.get(generic);
			if (observableList == null)
				dependenciesPromiseAsOservableListCacheMap.put(generic, observableList = new GSListBinding(generic, dependenciesPromise));
			observableList.push(snapshot.toList());
			return observableList;
		});
	}

	private Map<Generic, GSListBinding> dependenciesPromiseAsOservableListCacheMap = new HashMap<Generic, GSListBinding>();

	private class GSListBinding extends ListBinding<Generic> {
		private final ObjectBinding<ObservableValue<CompletableFuture<Snapshot<Generic>>>> binding;
		private Collection<? extends Generic> elements = new ArrayList<>();
		@SuppressWarnings("unused")
		private ChangeListener<ObservableValue<CompletableFuture<Snapshot<Generic>>>> bindingListener;

		private GSListBinding(Generic generic, ObservableValue<CompletableFuture<Snapshot<Generic>>> dependenciesPromise) {
			this.binding = new ObjectBinding<ObservableValue<CompletableFuture<Snapshot<Generic>>>>() {

				private ObservableValue<CompletableFuture<Snapshot<Generic>>> currentBindedDifferential;

				{
					currentBindedDifferential = dependenciesPromise;
					bind(differentialProperty, currentBindedDifferential);
				}

				@Override
				protected ObservableValue<CompletableFuture<Snapshot<Generic>>> computeValue() {
					return currentBindedDifferential;
				}

				@Override
				protected void onInvalidating() {
					unbind(currentBindedDifferential);
					currentBindedDifferential = getDifferential().getDependenciesPromise(generic);
					bind(currentBindedDifferential);
				};
			};

			binding.addListener(new WeakChangeListener<>(bindingListener = ((observable, oldV, newV) -> {
				newV.getValue().thenAccept(snapshot -> {
					elements = snapshot.toList();
					invalidate();
				});
			})));
		}

		@Override
		protected ObservableList<Generic> computeValue() {
			return FXCollections.observableArrayList(elements);

		}

		public boolean push(Collection<? extends Generic> elements) {
			this.elements = elements;
			invalidate();
			return true;
		};

	}

}
