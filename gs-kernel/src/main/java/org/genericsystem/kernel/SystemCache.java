package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.api.core.IRoot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator;
import org.genericsystem.api.core.annotations.constraints.NoReferentialIntegrityProperty;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.RequiredConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.constraints.UniqueValueConstraint;
import org.genericsystem.api.core.annotations.value.AxedPropertyClassValue;
import org.genericsystem.api.core.annotations.value.BooleanValue;
import org.genericsystem.api.core.annotations.value.ByteArrayValue;
import org.genericsystem.api.core.annotations.value.DoubleValue;
import org.genericsystem.api.core.annotations.value.EngineValue;
import org.genericsystem.api.core.annotations.value.FloatValue;
import org.genericsystem.api.core.annotations.value.IntValue;
import org.genericsystem.api.core.annotations.value.LongValue;
import org.genericsystem.api.core.annotations.value.ShortValue;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.api.core.exceptions.CyclicException;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.kernel.GenericHandler.SetSystemHandler;

public class SystemCache<T extends DefaultVertex<T>> {

	private final Map<Class<?>, T> systemCache = new HashMap<>();

	private final Map<T, Class<?>> reverseSystemCache = new IdentityHashMap<>();

	protected final AbstractRoot<T> root;

	public SystemCache(AbstractRoot<T> root, Class<?> rootClass) {
		this.root = root;
		put(DefaultRoot.class, (T) root);
		put(Root.class, (T) root);
		put(rootClass, (T) root);
	}

	public void mount(List<Class<?>> systemClasses, Class<?>... userClasses) {
		for (Class<?> clazz : systemClasses)
			bind(clazz);
		for (Class<?> clazz : userClasses)
			bind(clazz);
	}

	private T set(Class<?> clazz) {
		if (root.isInitialized())
			throw new IllegalStateException("Class : " + clazz + " has not been built at startup");
		T systemProperty = systemCache.get(clazz);
		if (systemProperty != null) {
			assert systemProperty.isAlive();
			return systemProperty;
		}
		T meta = setMeta(clazz);
		List<T> overrides = setOverrides(clazz);
		List<T> components = setComponents(clazz);
		systemProperty = new SetSystemHandler<T>(root.getCurrentCache(), clazz, meta, overrides, findValue(clazz), components).resolve();
		put(clazz, systemProperty);
		mountConstraints(clazz, systemProperty);
		triggersDependencies(clazz);

		return systemProperty;
	}

	private void put(Class<?> clazz, T vertex) {
		systemCache.put(clazz, vertex);
		reverseSystemCache.put(vertex, clazz);
	}

	public T find(Class<?> clazz) {
		if (IRoot.class.isAssignableFrom(clazz))
			return (T) root;
		return systemCache.get(clazz);
	}

	public T bind(Class<?> clazz) {
		T result = find(clazz);
		if (result == null)
			result = set(clazz);
		return result;
	}

	public Class<?> getClassByVertex(T vertex) {
		return reverseSystemCache.get(vertex);
	}

	void mountConstraints(Class<?> clazz, T result) {
		if (clazz.getAnnotation(PropertyConstraint.class) != null)
			result.enablePropertyConstraint();

		if (clazz.getAnnotation(UniqueValueConstraint.class) != null)
			result.enableUniqueValueConstraint();

		if (clazz.getAnnotation(InstanceValueClassConstraint.class) != null)
			result.setInstanceValueClassConstraint(clazz.getAnnotation(InstanceValueClassConstraint.class).value());

		if (clazz.getAnnotation(InstanceValueGenerator.class) != null)
			result.setInstanceValueGenerator(clazz.getAnnotation(InstanceValueGenerator.class).value());

		RequiredConstraint requiredConstraint = clazz.getAnnotation(RequiredConstraint.class);
		if (requiredConstraint != null)
			for (int axe : requiredConstraint.value()) {
				result.enableRequiredConstraint(axe);
				// assert result.isRequiredConstraintEnabled(axe) : result.getComposites().first().info();
			}

		NoReferentialIntegrityProperty referentialIntegrity = clazz.getAnnotation(NoReferentialIntegrityProperty.class);
		if (referentialIntegrity != null)
			for (int axe : referentialIntegrity.value())
				result.disableReferentialIntegrity(axe);

		SingularConstraint singularTarget = clazz.getAnnotation(SingularConstraint.class);
		if (singularTarget != null)
			for (int axe : singularTarget.value())
				result.enableSingularConstraint(axe);
	}

	private void triggersDependencies(Class<?> clazz) {
		Dependencies dependenciesClass = clazz.getAnnotation(Dependencies.class);
		if (dependenciesClass != null)
			for (Class<?> dependencyClass : dependenciesClass.value())
				bind(dependencyClass);
	}

	private T setMeta(Class<?> clazz) {
		Meta meta = clazz.getAnnotation(Meta.class);
		if (meta == null)
			return (T) root;
		if (meta.value() == clazz)
			return null;
		return bind(meta.value());
	}

	private List<T> setOverrides(Class<?> clazz) {
		List<T> overridesVertices = new ArrayList<>();
		org.genericsystem.api.core.annotations.Supers supersAnnotation = clazz.getAnnotation(org.genericsystem.api.core.annotations.Supers.class);
		if (supersAnnotation != null)
			for (Class<?> overrideClass : supersAnnotation.value())
				overridesVertices.add(bind(overrideClass));
		return overridesVertices;
	}

	private Serializable findValue(Class<?> clazz) {
		AxedPropertyClassValue axedPropertyClass = clazz.getAnnotation(AxedPropertyClassValue.class);
		if (axedPropertyClass != null)
			return new org.genericsystem.api.core.AxedPropertyClass(axedPropertyClass.propertyClass(), axedPropertyClass.pos());

		BooleanValue booleanValue = clazz.getAnnotation(BooleanValue.class);
		if (booleanValue != null)
			return booleanValue.value();

		ByteArrayValue byteArrayValue = clazz.getAnnotation(ByteArrayValue.class);
		if (byteArrayValue != null)
			return byteArrayValue.value();

		DoubleValue doubleValue = clazz.getAnnotation(DoubleValue.class);
		if (doubleValue != null)
			return doubleValue.value();

		EngineValue engineValue = clazz.getAnnotation(EngineValue.class);
		if (engineValue != null)
			return root.getValue();

		FloatValue floatValue = clazz.getAnnotation(FloatValue.class);
		if (floatValue != null)
			return floatValue.value();

		IntValue intValue = clazz.getAnnotation(IntValue.class);
		if (intValue != null)
			return intValue.value();

		LongValue longValue = clazz.getAnnotation(LongValue.class);
		if (longValue != null)
			return longValue.value();

		ShortValue shortValue = clazz.getAnnotation(ShortValue.class);
		if (shortValue != null)
			return shortValue.value();

		StringValue stringValue = clazz.getAnnotation(StringValue.class);
		if (stringValue != null)
			return stringValue.value();

		return clazz;
	}

	private List<T> setComponents(Class<?> clazz) {
		List<T> components = new ArrayList<>();
		Components componentsAnnotation = clazz.getAnnotation(Components.class);
		if (componentsAnnotation != null)
			for (Class<?> compositeClass : componentsAnnotation.value())
				if (compositeClass.equals(clazz))
					root.getCurrentCache().discardWithException(new CyclicException("The annoted class " + clazz + " has a component with same name"));
				else
					components.add(set(compositeClass));
		return components;
	}
}
