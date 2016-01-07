package org.genericsystem.defaults.async;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.DefaultVertex;

public class AsyncSupersComputer<T extends DefaultVertex<T>> {

	private final T meta;
	private final List<T> overrides;
	private final List<T> components;
	private final Serializable value;

	private final int reachLevel;

	private final LinkedHashSet<T> set = new LinkedHashSet<>();
	private final CompletableFuture<Void> state;

	private final Map<T, boolean[]> alreadyComputed = new HashMap<>();

	public AsyncSupersComputer(T meta, List<T> overrides, Serializable value, List<T> components) {
		assert meta != null;
		reachLevel = isMeta(meta, value, components) ? ApiStatics.META : meta.getLevel() + 1;
		this.meta = meta;
		this.overrides = overrides;
		this.components = components;
		this.value = value;
		state = asyncVisitSupers(CompletableFuture.completedFuture(null), meta);
	}

	public CompletableFuture<LinkedHashSet<T>> get() {
		return state.thenApply(s -> set);
	}

	private boolean isMeta(T meta, Serializable value, List<T> components) {
		return meta.isMeta() && components.stream().allMatch(c -> c != null && c.isRoot()) && Objects.equals(value, meta.getRoot().getValue());
	}

	private CompletableFuture<Void> asyncVisitSupers(CompletableFuture<Void> trigger, T candidate) {
		if (candidate.getSupers().isEmpty())
			return asyncVisit(trigger, candidate).thenApply(result -> null);

		for (T superOfCandidate : candidate.getSupers())
			trigger = asyncVisitSupers(trigger, superOfCandidate);
		return trigger;
	}

	private CompletableFuture<boolean[]> asyncVisit(CompletableFuture<Void> trigger, T candidate) {
		return trigger.thenCompose(s -> {
			boolean[] result = alreadyComputed.get(candidate);
			if (result != null)
				return CompletableFuture.completedFuture(result);

			boolean isMeta = meta.isSpecializationOf(candidate);
			boolean isSuper = !isMeta && candidate.isSuperOf(meta, overrides, value, components);
			if (!isMeta && !isSuper) {
				boolean[] selectableSelected = new boolean[] { true, false };
				alreadyComputed.put(candidate, selectableSelected);
				return CompletableFuture.completedFuture(selectableSelected);
			}

			CompletableFuture<Boolean> selectablePromise = candidate.getAsyncInheritings().thenCompose(
					inheritings -> {
						CompletableFuture<Boolean> internalSelectable = CompletableFuture.completedFuture(true);
						for (T inheriting : inheritings)
							internalSelectable = internalSelectable.thenCompose(internal -> asyncVisit(CompletableFuture.completedFuture(null), inheriting).thenApply(
									subSelectionableSelectioned -> subSelectionableSelectioned[0] || !subSelectionableSelectioned[1]).thenApply(visitResult -> !internal ? false : visitResult));
						return internalSelectable;
					});

			if (isMeta) {
				selectablePromise = selectablePromise.thenCompose(selectable -> candidate.getAsyncInstances().thenCompose(
						instances -> {
							CompletableFuture<Boolean> internalSelectable = CompletableFuture.completedFuture(selectable);
							for (T instance : instances)
								internalSelectable = internalSelectable.thenCompose(internal -> asyncVisit(CompletableFuture.completedFuture(null), instance).thenApply(
										subSelectionableSelectioned -> subSelectionableSelectioned[0] || !subSelectionableSelectioned[1]).thenApply(visitResult -> !internal ? false : visitResult));
							return internalSelectable;
						}));
			}

			return selectablePromise.thenApply(selectable -> {
				boolean[] selectableSelected = new boolean[] { selectable, false };
				alreadyComputed.put(candidate, selectableSelected);
				assert result == null : candidate.info();
				if (selectableSelected[0] && (candidate.getLevel() == reachLevel) && !candidate.inheritsFrom(meta, overrides, value, components)) {
					set.add(candidate);
					selectableSelected[1] = true;
				}
				return selectableSelected;
			});
		});
	}
}
