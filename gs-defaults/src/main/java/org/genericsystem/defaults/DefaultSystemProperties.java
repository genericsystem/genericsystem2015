package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.Objects;
import java.util.stream.Stream;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.IVertex;
import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator.ValueGenerator;
import org.genericsystem.api.core.exceptions.NotFoundException;
import org.genericsystem.defaults.DefaultConfig.CascadeRemoveProperty;
import org.genericsystem.defaults.DefaultConfig.InstanceValueGeneratorProperty;
import org.genericsystem.defaults.DefaultConfig.NoReferentialIntegrityProperty;
import org.genericsystem.defaults.DefaultConfig.NonHeritableProperty;
import org.genericsystem.defaults.constraints.InstanceValueClassConstraint;
import org.genericsystem.defaults.constraints.PropertyConstraint;
import org.genericsystem.defaults.constraints.RequiredConstraint;
import org.genericsystem.defaults.constraints.SingularConstraint;
import org.genericsystem.defaults.constraints.UniqueValueConstraint;

public interface DefaultSystemProperties<T extends DefaultVertex<T>> extends IVertex<T> {

	@Override
	default Serializable getSystemPropertyValue(Class<? extends SystemProperty> propertyClass, int pos) {
		T key = getKey(propertyClass, pos);
		if (key != null) {
			T result = getHolders(key).stream().filter(x -> this.isSpecializationOf(x.getBaseComponent())).findFirst().orElse(null);
			return result != null ? result.getValue() : null;
		}
		return null;
	}

	@Override
	default T getKey(Class<? extends SystemProperty> propertyClass, int pos) {
		T property = getRoot().find(propertyClass);
		Stream<T> keys = property != null ? property.getInheritings().stream() : Stream.empty();
		return keys.filter(x -> Objects.equals(x.getValue(), new AxedPropertyClass(propertyClass, pos))).findFirst().orElse(null);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T setSystemPropertyValue(Class<? extends SystemProperty> propertyClass, int pos, Serializable value, T... targets) {
		if (pos != ApiStatics.NO_POSITION && getComponent(pos) == null)
			((DefaultContext<T>) getCurrentCache()).discardWithException(new NotFoundException("Unable to set system property : " + propertyClass.getSimpleName() + " because no component exists for position : " + pos));

		T map = getRoot().getMap();
		assert map != null;
		T property = getRoot().<T> bind(propertyClass);
		assert property != null : propertyClass;
		map.getMeta().setInstance(property, new AxedPropertyClass(propertyClass, pos), coerceToTArray(property.getComponents().toArray())).setInstance(value, addThisToTargets(targets));
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	default T enableSystemProperty(Class<? extends SystemProperty> propertyClass, int pos, T... targets) {
		setSystemPropertyValue(propertyClass, pos, Boolean.TRUE, targets);
		return (T) this;
	}

	@Override
	@SuppressWarnings("unchecked")
	default T disableSystemProperty(Class<? extends SystemProperty> propertyClass, int pos, T... targets) {
		setSystemPropertyValue(propertyClass, pos, Boolean.FALSE, targets);
		return (T) this;
	}

	@Override
	default boolean isSystemPropertyEnabled(Class<? extends SystemProperty> propertyClass, int pos) {
		Serializable value = getSystemPropertyValue(propertyClass, pos);
		return value != null && !Boolean.FALSE.equals(value);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T enableReferentialIntegrity(int pos) {
		return disableSystemProperty(NoReferentialIntegrityProperty.class, pos);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T disableReferentialIntegrity(int pos) {
		return enableSystemProperty(NoReferentialIntegrityProperty.class, pos);
	}

	@Override
	default boolean isReferentialIntegrityEnabled(int pos) {
		return !isSystemPropertyEnabled(NoReferentialIntegrityProperty.class, pos);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T enableSingularConstraint(int pos) {
		return enableSystemProperty(SingularConstraint.class, pos);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T disableSingularConstraint(int pos) {
		return disableSystemProperty(SingularConstraint.class, pos);
	}

	@Override
	default boolean isSingularConstraintEnabled(int pos) {
		return isSystemPropertyEnabled(SingularConstraint.class, pos);
	}

	@Override
	default T enablePropertyConstraint() {
		return enableSystemProperty(PropertyConstraint.class, ApiStatics.NO_POSITION);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T disablePropertyConstraint() {
		return disableSystemProperty(PropertyConstraint.class, ApiStatics.NO_POSITION);
	}

	@Override
	default boolean isPropertyConstraintEnabled() {
		return isSystemPropertyEnabled(PropertyConstraint.class, ApiStatics.NO_POSITION);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T enableUniqueValueConstraint() {
		return enableSystemProperty(UniqueValueConstraint.class, ApiStatics.NO_POSITION);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T disableUniqueValueConstraint() {
		return disableSystemProperty(UniqueValueConstraint.class, ApiStatics.NO_POSITION);
	}

	@Override
	default boolean isUniqueValueEnabled() {
		return isSystemPropertyEnabled(UniqueValueConstraint.class, ApiStatics.NO_POSITION);
	}

	@Override
	default T enableRequiredConstraint(int pos) {
		return enableSystemProperty(RequiredConstraint.class, pos, coerceToTArray(this.getComponents().get(pos)));
	}

	@Override
	default T disableRequiredConstraint(int pos) {
		return disableSystemProperty(RequiredConstraint.class, pos, coerceToTArray(this.getComponents().get(pos)));
	}

	@Override
	default boolean isRequiredConstraintEnabled(int pos) {
		return isSystemPropertyEnabled(RequiredConstraint.class, pos);
	}

	@SuppressWarnings("unchecked")
	@Override
	@Deprecated
	default T enableCascadeRemove(int pos) {
		return enableSystemProperty(CascadeRemoveProperty.class, pos);
	}

	@Deprecated
	@SuppressWarnings("unchecked")
	@Override
	default T disableCascadeRemove(int pos) {
		return disableSystemProperty(CascadeRemoveProperty.class, pos);
	}

	@Deprecated
	@Override
	default boolean isCascadeRemoveEnabled(int pos) {
		return isSystemPropertyEnabled(CascadeRemoveProperty.class, pos);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T enableInheritance() {
		return disableSystemProperty(NonHeritableProperty.class, ApiStatics.NO_POSITION);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T disableInheritance() {
		return enableSystemProperty(NonHeritableProperty.class, ApiStatics.NO_POSITION);
	}

	@Override
	default boolean isInheritanceEnabled() {
		return !isSystemPropertyEnabled(NonHeritableProperty.class, ApiStatics.NO_POSITION);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Class<? extends Serializable> getInstanceValueClassConstraint() {
		return (Class<? extends Serializable>) getSystemPropertyValue(InstanceValueClassConstraint.class, ApiStatics.NO_POSITION);
	}

	@Override
	// @SuppressWarnings("unchecked")
	default T setInstanceValueClassConstraint(Class<? extends Serializable> instanceValueConstraintClass) {
		setSystemPropertyValue(InstanceValueClassConstraint.class, ApiStatics.NO_POSITION, instanceValueConstraintClass);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	default Class<? extends ValueGenerator> getInstanceValueGenerator() {
		return (Class<? extends ValueGenerator>) getSystemPropertyValue(InstanceValueGeneratorProperty.class, ApiStatics.NO_POSITION);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setInstanceValueGenerator(Class<? extends ValueGenerator> instanceValueGeneratorClass) {
		setSystemPropertyValue(InstanceValueGeneratorProperty.class, ApiStatics.NO_POSITION, instanceValueGeneratorClass);
		return (T) this;
	}

}
