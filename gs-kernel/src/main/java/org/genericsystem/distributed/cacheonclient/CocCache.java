package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

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
		public ObservableValue<CompletableFuture<Snapshot<Generic>>> getDependenciesPromise(Generic generic) {
			return Bindings.<CompletableFuture<Snapshot<Generic>>> createObjectBinding(() -> getTransaction().getDependenciesPromise(generic), transactionProperty);
		}

		@Override
		public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic) {
			return getTransaction().getDependenciesObservableSnapshot(generic);
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

	public CompletableFuture<ObservableList<Generic>> getDependenciesPromise(Generic generic) {
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
					System.out.println("(CocCache - GSListBinding) Invalidation of binded differential");
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
		protected void onInvalidating() {
			System.out.println("(CocCache - GSListBinding) Invalidation of elements' Collection");
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

	public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
		return getDifferential().getDependenciesObservableSnapshot(generic).toObservableList();
	}

}
