package org.genericsystem.kernel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.NavigableSet;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.kernel.GenericHandler.AtomicHandler;

public class Restructurator<T extends DefaultVertex<T>> {

	private final AbstractContext<T> context;

	Restructurator(AbstractContext<T> context) {
		this.context = context;
	}

	T rebuildAll(T toRebuild, Supplier<T> rebuilder, NavigableSet<T> dependenciesToRebuild) {
		dependenciesToRebuild.descendingSet().forEach(context::unplug);
		if (rebuilder != null) {
			ConvertMap convertMap = new ConvertMap();
			T build = rebuilder.get();
			if (toRebuild != null) {
				dependenciesToRebuild.remove(toRebuild);
				convertMap.put(toRebuild, build);
			}
			dependenciesToRebuild.forEach(x -> convertMap.convert(x));
			return build;
		}
		return null;
	}

	private class ConvertMap extends HashMap<T, T> {
		private static final long serialVersionUID = 5003546962293036021L;

		private T convert(T oldDependency) {
			if (oldDependency.isAlive())
				return oldDependency;
			T newDependency = get(oldDependency);
			if (newDependency == null) {
				if (oldDependency.isMeta()) {
					assert oldDependency.getSupers().size() == 1;
					newDependency = context.setMeta(oldDependency.getComponents().size());
				} else {
					List<T> overrides = reasignSupers(oldDependency, new ArrayList<>());
					List<T> components = reasignComponents(oldDependency);
					T meta = reasignMeta(components, convert(oldDependency.getMeta()));
					newDependency = new AtomicHandler<>(context, meta, overrides, oldDependency.getValue(), components).resolve();
				}
				put(oldDependency, newDependency);// triggers mutation
			}
			return newDependency;
		}

		private List<T> reasignSupers(T oldDependency, List<T> supersReasign) {
			for (T ancestor : oldDependency.getSupers().stream().map(x -> convert(x)).collect(Collectors.toList()))
				if (!ancestor.isAlive())
					reasignSupers(ancestor, supersReasign);
				else
					supersReasign.add(ancestor);
			return supersReasign;
		}

		private List<T> reasignComponents(T oldDependency) {
			return oldDependency.getComponents().stream().map(x -> convert(x)).filter(x -> x.isAlive()).collect(Collectors.toList());
		}

		private T reasignMeta(List<T> components, T meta) {
			if (components.size() != meta.getComponents().size())
				return reasignMeta(components, meta.getSupers().get(0));
			return meta;
		}

		@Override
		public T put(T oldDependency, T newDependency) {
			T result = super.put(oldDependency, newDependency);
			context.triggersMutation(oldDependency, newDependency);
			return result;
		}
	}
}
