package org.genericsystem.reactor;

import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;

import io.reactivex.Observable;

public class MetaBinding<BETWEEN> {
	private Function<Context, Observable<Snapshot<BETWEEN>>> betweenChildren;
	private final BiFunction<Context, BETWEEN, Context> modelBuilder;

	private static BiFunction<Context, Generic, Context> MODEL_BUILDER = (model, generic) -> new Context(model, Context.addToGenerics(generic, model.getGenerics()));
	private static BiFunction<Context, Context, Context> MODEL_CLONER = (model, subModel) -> new Context(model, subModel.getGenerics());

	public MetaBinding(Function<Context, Observable<Snapshot<BETWEEN>>> betweenChildren, BiFunction<Context, BETWEEN, Context> modelBuilder) {
		this.betweenChildren = betweenChildren;
		this.modelBuilder = modelBuilder;
	}

	private Observable<Snapshot<Context>> buildChildren(Context context, Tag childTag) {
		return betweenChildren.apply(context).map(s -> s.map(g -> modelBuilder.apply(context, g)));
	}

	public Observable<IndexedSubContext> buildFilteredChildren(Context context, Tag childTag) {
		return buildChildren(context, childTag).doOnNext(s -> {
			// Delete existing subContexts when a new Snapshot is received.
			context.getSubContexts(childTag).forEach(sc -> sc.destroy());
			context.getSubContexts(childTag).clear();
		}).switchMap(s -> s.getIndexedElements().flatMap(indexedSubContext -> buildIndexedFilteredObservable(s, indexedSubContext.getElement(), childTag, indexedSubContext.getIndex())));
	}

	@SuppressWarnings("unchecked")
	private Observable<IndexedSubContext> buildIndexedFilteredObservable(Snapshot<Context> snapshot, Context subContext, Tag childTag, int index) {
		return RxJavaHelpers.changesOf(childTag.getObservableSwitchers()).switchMap(list -> list.isEmpty() ? Observable.just(new IndexedSubContext(subContext, true, index)) :
			Observable.combineLatest(list.stream().map(switcher -> RxJavaHelpers.valuesOf(switcher.apply(subContext, childTag))).toArray(Observable[]::new),
					args -> new IndexedSubContext(subContext, Arrays.stream(args).allMatch(v -> Boolean.TRUE.equals(v)), index)).distinctUntilChanged())
				.mergeWith(snapshot.getRemovesObservable().filter(c -> c.equals(subContext)).map(c -> new IndexedSubContext(c, false, -1)))
				.takeUntil(ic -> ((IndexedSubContext) ic).getIndex() < 0);
	}

	public Function<Context, Observable<Snapshot<BETWEEN>>> getBetweenChildren() {
		return betweenChildren;
	}

	public static MetaBinding<Context> selectMetaBinding(Function<Context, Observable<Snapshot<Context>>> betweenChildren) {
		return new MetaBinding<Context>(betweenChildren, MODEL_CLONER);
	}

	public static MetaBinding<Generic> forEachMetaBinding(Function<Context, Observable<Snapshot<Generic>>> betweenChildren) {
		return new MetaBinding<Generic>(betweenChildren, MODEL_BUILDER);
	}

	public void setBetweenChildren(Function<Context, Observable<Snapshot<BETWEEN>>> betweenChildren) {
		this.betweenChildren = betweenChildren;
	}

	public static class IndexedSubContext {
		private Context context;
		private boolean create;
		private int index;

		public IndexedSubContext(Context context, boolean create, int index) {
			this.context = context;
			this.create = create;
			this.index = index;
		}

		public Context getContext() {
			return context;
		}

		public boolean getCreate() {
			return create;
		}

		public int getIndex() {
			return index;
		}

		@Override
		public int hashCode() {
			return context.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof IndexedSubContext))
				return false;
			IndexedSubContext other = (IndexedSubContext) obj;
			return create == other.create && context.equals(other.context) && index == other.index;
		}
	}
}