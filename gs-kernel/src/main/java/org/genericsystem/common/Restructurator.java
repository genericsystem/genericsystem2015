package org.genericsystem.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.NavigableSet;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.genericsystem.common.GenericBuilder.AtomicBuilder;

public class Restructurator {

	private final AbstractCache context;

	Restructurator(AbstractCache context) {
		this.context = context;
	}

	Generic rebuildAll(Generic toRebuild, Supplier<Generic> rebuilder, NavigableSet<Generic> dependenciesToRebuild) {
		dependenciesToRebuild.descendingSet().forEach(context::unplug);
		if (rebuilder != null) {
			ConvertMap convertMap = new ConvertMap();
			Generic build = rebuilder.get();
			if (toRebuild != null) {
				dependenciesToRebuild.remove(toRebuild);
				convertMap.put(toRebuild, build);
			}
			dependenciesToRebuild.forEach(x -> convertMap.convert(x));
			return build;
		}
		return null;
	}

	private class ConvertMap extends HashMap<Generic, Generic> {
		private static final long serialVersionUID = 5003546962293036021L;

		private Generic convert(Generic oldDependency) {
			if (oldDependency.isAlive())
				return oldDependency;
			Generic newDependency = get(oldDependency);
			if (newDependency == null) {
				if (oldDependency.isMeta()) {
					assert oldDependency.getSupers().size() == 1;
					newDependency = context.setMeta(oldDependency.getComponents().size());
				} else {
					List<Generic> overrides = reasignSupers(oldDependency, new ArrayList<>());
					List<Generic> components = reasignComponents(oldDependency);
					Generic meta = reasignMeta(components, convert(oldDependency.getMeta()));
					newDependency = new AtomicBuilder(context, meta, overrides, oldDependency.getValue(), components).resolve();
				}
				put(oldDependency, newDependency);// triggers mutation
			}
			return newDependency;
		}

		private List<Generic> reasignSupers(Generic oldDependency, List<Generic> supersReasign) {
			for (Generic ancestor : oldDependency.getSupers().stream().map(x -> convert(x)).collect(Collectors.toList()))
				if (!ancestor.isAlive())
					reasignSupers(ancestor, supersReasign);
				else
					supersReasign.add(ancestor);
			return supersReasign;
		}

		private List<Generic> reasignComponents(Generic oldDependency) {
			return oldDependency.getComponents().stream().map(x -> convert(x)).filter(x -> x.isAlive()).collect(Collectors.toList());
		}

		private Generic reasignMeta(List<Generic> components, Generic meta) {
			if (components.size() != meta.getComponents().size())
				return reasignMeta(components, meta.getSupers().get(0));
			return meta;
		}

		@Override
		public Generic put(Generic oldDependency, Generic newDependency) {
			Generic result = super.put(oldDependency, newDependency);
			context.triggersMutation(oldDependency, newDependency);
			return result;
		}
	}
}
