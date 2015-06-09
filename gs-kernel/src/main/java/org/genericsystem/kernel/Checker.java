package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.annotations.Priority;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.api.core.exceptions.CrossEnginesAssignementsException;
import org.genericsystem.api.core.exceptions.LevelConstraintViolationException;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.genericsystem.api.core.exceptions.NotAliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.NotAllowedSerializableTypeException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint;

public class Checker<T extends DefaultVertex<T>> {

	private final AbstractContext<T> context;

	public Checker(AbstractContext<T> context) {
		this.context = context;
	}

	public AbstractContext<T> getContext() {
		return context;
	}

	public void checkBeforeBuild(T meta, List<T> overrides, Serializable value, List<T> components) throws RollbackException {
		checkSystemConstraintsBeforeBuild(meta, overrides, value, components);
	}

	public void checkAfterBuild(boolean isOnAdd, boolean isFlushTime, T vertex) throws RollbackException {
		checkSystemConstraintsAfterBuild(isOnAdd, isFlushTime, vertex);
		checkConsistency(vertex);
		checkConstraints(isOnAdd, isFlushTime, vertex);
	}

	// checkBeforeBuild

	private void checkSystemConstraintsBeforeBuild(T meta, List<T> overrides, Serializable value, List<T> components) {
		checkSameEngine(meta, overrides, components);
		checkIsAlive(meta, overrides, components);
		checkSerializableType(value);
		checkWellFormedMeta(meta, value, components);
	}

	private void checkSameEngine(T meta, List<T> overrides, List<T> components) {
		if (meta == null)
			return;
		DefaultRoot<T> root = meta.getRoot();
		for (T component : components)
			if (!root.equals(component.getRoot()))
				context.discardWithException(new CrossEnginesAssignementsException("Unable to associate meta " + meta + " with his component " + component + " because they are from differents engines"));
		for (T directSuper : overrides)
			if (directSuper != null && !root.equals(directSuper.getRoot()))
				context.discardWithException(new CrossEnginesAssignementsException("Unable to associate meta " + meta + " with his super " + directSuper + " because they are from differents engines"));
	}

	private void checkIsAlive(T meta, List<T> overrides, List<T> components) {
		if (meta != null)
			checkIsAlive(meta);
		overrides.forEach(x -> checkIsAlive(x));
		components.forEach(x -> checkIsAlive(x));
	}

	private void checkSerializableType(Serializable value) {
		if (value == null)
			return;
		if (value instanceof org.genericsystem.api.core.AxedPropertyClass)
			return;
		if (value instanceof Boolean)
			return;
		if (value instanceof byte[])
			return;
		if (value instanceof Double)
			return;
		if (value instanceof Float)
			return;
		if (value instanceof Integer)
			return;
		if (value instanceof Long)
			return;
		if (value instanceof Short)
			return;
		if (value instanceof String)
			return;
		if (value instanceof Class)
			return;
		context.discardWithException(new NotAllowedSerializableTypeException("Not allowed type for your serializable. Only primitive and Byte[] allowed."));
	}

	private void checkWellFormedMeta(T meta, Serializable value, List<T> components) {
		if (meta == null && (components.stream().anyMatch(x -> !x.isRoot()) || !Objects.equals(value, context.getRoot().getValue())))
			context.discardWithException(new IllegalStateException("Malformed meta : (" + meta + ") " + value + " " + components));
	}

	// checkAfterBuild

	private void checkWellFormedMeta(T vertex) {
		if (vertex.isMeta())
			if (vertex.getComponents().stream().anyMatch(c -> !c.isRoot()) || !Objects.equals(vertex.getValue(), context.getRoot().getValue()) || vertex.getSupers().size() != 1 || !vertex.getSupers().get(0).isMeta())
				context.discardWithException(new IllegalStateException("Malformed meta : " + vertex.info()));
	}

	protected void checkSystemConstraintsAfterBuild(boolean isOnAdd, boolean isFlushTime, T vertex) {
		checkWellFormedMeta(vertex);
		if (isOnAdd || !isFlushTime)
			checkIsAlive(vertex);
		else
			checkIsNotAlive(vertex);
		if (!isOnAdd)
			checkDependenciesAreEmpty(vertex);
		checkSameEngine(vertex);
		checkDependsMetaComponents(vertex);
		checkSupers(vertex);
		checkDependsSuperComponents(vertex);
		checkLevel(vertex);
		checkLevelComponents(vertex);
		checkSignatureUnicity(vertex);
		checkRemoveGenericAnnoted(isOnAdd, vertex);
	}

	private void checkRemoveGenericAnnoted(boolean isOnAdd, T vertex) {
		if (!isOnAdd && vertex.isSystem())
			getContext().discardWithException(new IllegalAccessException("System node can't be removed " + vertex.info()));
	}

	void checkIsAlive(T vertex) {
		if (!context.isAlive(vertex))
			context.discardWithException(new AliveConstraintViolationException(vertex.info()));
	}

	private void checkIsNotAlive(T vertex) {
		if (context.isAlive(vertex))
			context.discardWithException(new NotAliveConstraintViolationException(vertex.info()));
	}

	private void checkDependenciesAreEmpty(T vertex) {
		if (!context.getDependencies(vertex).isEmpty())
			context.discardWithException(new ReferentialIntegrityConstraintViolationException("Unable to remove : " + vertex.info() + " cause it has dependencies"));
	}

	private void checkSameEngine(T vertex) {
		DefaultRoot<T> root = vertex.getRoot();
		for (T component : vertex.getComponents())
			if (!root.equals(component.getRoot()))
				context.discardWithException(new CrossEnginesAssignementsException("Unable to associate his " + vertex + " with his component " + component + " because they are from differents engines"));
		for (T directSuper : vertex.getSupers())
			if (directSuper != null && !root.equals(directSuper.getRoot()))
				context.discardWithException(new CrossEnginesAssignementsException("Unable to associate his " + vertex + " with his super " + directSuper + " because they are from differents engines"));
	}

	private void checkDependsMetaComponents(T vertex) {
		if (vertex.getMeta().getComponents().size() != vertex.getComponents().size())
			context.discardWithException(new MetaRuleConstraintViolationException("Added generic and its meta do not have the same components size. Added node components : " + vertex.getComponents() + " and meta components : "
					+ vertex.getMeta().getComponents()));
		for (int pos = 0; pos < vertex.getComponents().size(); pos++) {
			T component = vertex.getComponent(pos);
			T metaComponent = vertex.getMeta().getComponent(pos);
			if (component == null)
				if (metaComponent == null)
					continue;
				else
					component = vertex;
			else if (metaComponent == null)
				metaComponent = vertex.getMeta();
			if (!component.isInstanceOf(metaComponent) && !component.inheritsFrom(metaComponent))
				context.discardWithException(new MetaRuleConstraintViolationException("Component of added generic : " + component + " must be instance of or must inherits from the component of its meta : " + metaComponent));
		}
	}

	private void checkSupers(T vertex) {
		if (!vertex.getSupers().stream().allMatch(superVertex -> superVertex.getLevel() == vertex.getLevel()))
			context.discardWithException(new IllegalStateException("Inconsistant supers (bad level) : " + vertex.getSupers()));
		if (!vertex.getSupers().stream().allMatch(superVertex -> vertex.getMeta().inheritsFrom(superVertex.getMeta())))
			context.discardWithException(new IllegalStateException("Inconsistant supers : " + vertex.getSupers()));
		if (!vertex.getSupers().stream().noneMatch(this::equals))
			context.discardWithException(new IllegalStateException("Supers loop detected : " + vertex.info()));
		// if (vertex.getSupers().stream().anyMatch(superVertex -> Objects.equals(superVertex.getValue(), vertex.getValue()) && superVertex.getComponents().equals(vertex.getComponents()) && vertex.getMeta().inheritsFrom(superVertex.getMeta())))
		// context.discardWithException(new CollisionException("Collision detected : " + vertex.info() + " A collision occurs when two generics have same value and components and have same meta or metas that inherit one to another"));
	}

	private void checkDependsSuperComponents(T vertex) {
		vertex.getSupers().forEach(superVertex -> {
			if (!superVertex.isSuperOf(vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents()))
				context.discardWithException(new IllegalStateException("Inconsistant components : " + vertex.getComponents()));
		});
	}

	private void checkLevel(T vertex) {
		if (vertex.getLevel() > ApiStatics.CONCRETE)
			context.discardWithException(new LevelConstraintViolationException("Unable to instanciate a concrete generic : " + vertex.getMeta()));
	}

	private void checkLevelComponents(T vertex) {
		for (T component : vertex.getComponents())
			if (component.getLevel() > vertex.getLevel())
				context.discardWithException(new LevelConstraintViolationException("Inappropriate component meta level : " + component.getLevel() + " for component : " + component + ". Component meta level for added node is : " + vertex.getLevel()));
	}

	private void checkSignatureUnicity(T vertex) {
		// if (context.getInstances(vertex.getMeta()).get().filter(x -> ((AbstractVertex<?>) x).equalsRegardlessSupers(vertex.getMeta(), vertex.getValue(), vertex.getComponents())).count() > 1)
		// context.discardWithException(new ExistsException(vertex.info()));
	}

	private void checkConstraints(boolean isOnAdd, boolean isFlushTime, T vertex) {
		T map = getContext().getRoot().getMap();
		if (map != null) {
			Stream<T> contraintsHolders = vertex.getMeta().getHolders(map).stream()
					.filter(holder -> holder.getMeta().getValue() instanceof AxedPropertyClass && Constraint.class.isAssignableFrom(((AxedPropertyClass) holder.getMeta().getValue()).getClazz()))
					.filter(holder -> holder.getValue() != null && !Boolean.FALSE.equals(holder.getValue())).sorted(CONSTRAINT_PRIORITY);
			contraintsHolders.forEach(constraintHolder -> {
				T baseComponent = constraintHolder.getBaseComponent();
				if (vertex.isSpecializationOf(baseComponent))
					check(constraintHolder, baseComponent, isFlushTime, isOnAdd, false, vertex);
				T targetComponent = constraintHolder.getTargetComponent();
				if (targetComponent != null && vertex.isSpecializationOf(targetComponent))
					check(constraintHolder, baseComponent, isFlushTime, isOnAdd, true, vertex);
			});
		}
	}

	private void check(T constraintHolder, T baseComponent, boolean isFlushTime, boolean isOnAdd, boolean isRevert, T vertex) {
		try {
			statelessConstraint(constraintHolder.getMeta()).check(vertex, baseComponent, constraintHolder.getValue(), ((AxedPropertyClass) constraintHolder.getMeta().getValue()).getAxe(), isOnAdd, isFlushTime, isRevert);
		} catch (ConstraintViolationException e) {
			context.discardWithException(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Constraint<T> statelessConstraint(T vertex) {
		try {
			return (Constraint<T>) ((AxedPropertyClass) vertex.getValue()).getClazz().newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			context.discardWithException(e);
		}
		return null;
	}

	private int getConstraintPriority(T vertex) {
		Class<?> clazz = ((AxedPropertyClass) vertex.getValue()).getClazz();
		Priority priority = clazz.getAnnotation(Priority.class);
		return priority != null ? priority.value() : 0;
	}

	private void checkConsistency(T vertex) {
		T map = getContext().getRoot().find(SystemMap.class);
		if (map != null && vertex.isInstanceOf(map) && vertex.getMeta().getValue() instanceof AxedPropertyClass && Constraint.class.isAssignableFrom(((AxedPropertyClass) vertex.getMeta().getValue()).getClazz()) && vertex.getValue() != null
				&& !Boolean.FALSE.equals(vertex.getValue())) {
			T baseConstraint = vertex.getComponent(ApiStatics.BASE_POSITION);
			int axe = ((AxedPropertyClass) vertex.getMeta().getValue()).getAxe();
			if (((AxedPropertyClass) vertex.getMeta().getValue()).getAxe() == ApiStatics.NO_POSITION)
				baseConstraint.getSubInstances().forEach(x -> check(vertex, baseConstraint, true, true, false, x));
			else
				baseConstraint.getComponents().get(axe).getSubInstances().forEach(x -> check(vertex, baseConstraint, true, true, true, x));
		}
	}

	private final Comparator<T> CONSTRAINT_PRIORITY = new Comparator<T>() {
		@Override
		public int compare(T constraintHolder, T compareConstraintHolder) {
			return getConstraintPriority(constraintHolder.getMeta()) < getConstraintPriority(compareConstraintHolder.getMeta()) ? -1 : 1;
		}
	};

}
