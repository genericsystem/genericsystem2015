package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.DefaultVertex;

public class SupersComputer<T extends DefaultVertex<T>> extends LinkedHashSet<T> {

	private static final long serialVersionUID = -1078004898524170057L;

	private final T meta;
	private final List<T> overrides;
	private final List<T> components;
	private final Serializable value;

	private final int reachLevel;

	private final Map<T, boolean[]> alreadyComputed = new HashMap<>();

	SupersComputer(T meta, List<T> overrides, Serializable value, List<T> components) {
		assert meta != null;
		reachLevel = isMeta(meta, value, components) ? ApiStatics.META : meta.getLevel() + 1;
		this.meta = meta;
		this.overrides = overrides;
		this.components = components;
		this.value = value;
		visitSupers(meta);
	}

	private boolean isMeta(T meta, Serializable value, List<T> components) {
		return meta.isMeta() && components.stream().allMatch(c -> c != null && c.isRoot()) && Objects.equals(value, meta.getRoot().getValue());
	}

	private void visitSupers(T candidate) {
		if (candidate.getSupers().isEmpty())
			visit(candidate);
		else
			for (T superOfCandidate : candidate.getSupers())
				visitSupers(superOfCandidate);
	}

	private boolean[] visit(T candidate) {
		boolean[] result = alreadyComputed.get(candidate);
		if (result != null)
			return result;
		// boolean isMeta = meta == null ? false : meta.isSpecializationOf(candidate);
		boolean isMeta = meta.isSpecializationOf(candidate);
		boolean isSuper = !isMeta && candidate.isSuperOf(meta, overrides, value, components);
		if (!isMeta && !isSuper) {
			boolean[] selectableSelected = new boolean[] { true, false };
			alreadyComputed.put(candidate, selectableSelected);
			return selectableSelected;
		}
		boolean selectable = true;
		for (T inheriting : candidate.getInheritings()) {
			boolean[] subSelectionableSelectioned = visit(inheriting);
			if (!subSelectionableSelectioned[0] || subSelectionableSelectioned[1])
				selectable = false;
		}
		if (isMeta) {
			for (T instance : candidate.getInstances()) {
				boolean[] subSelectableSelected = visit(instance);
				if (!subSelectableSelected[0] || subSelectableSelected[1])
					selectable = false;
			}
		}
		boolean[] selectableSelected = new boolean[] { selectable, false };
		result = alreadyComputed.put(candidate, selectableSelected);
		assert result == null : candidate.info();
		if (selectableSelected[0] && (candidate.getLevel() == reachLevel) && !candidate.inheritsFrom(meta, overrides, value, components)) {
			add(candidate);
			selectableSelected[1] = true;
		}
		return selectableSelected;
	}
}
