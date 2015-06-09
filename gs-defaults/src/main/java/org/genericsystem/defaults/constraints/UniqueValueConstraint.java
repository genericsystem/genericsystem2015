package org.genericsystem.defaults.constraints;

import java.io.Serializable;
import java.util.Objects;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint.CheckedConstraint;
import org.genericsystem.defaults.exceptions.UniqueValueConstraintViolationException;

/**
 * Represents the constraint to allow only one value for an instance.
 *
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of DefaultVertex.
 */
@SystemGeneric
@Meta(MetaAttribute.class)
@Supers(SystemMap.class)
@Components(DefaultRoot.class)
@InstanceValueClassConstraint(Boolean.class)
@PropertyConstraint
public class UniqueValueConstraint<T extends DefaultVertex<T>> implements CheckedConstraint<T> {
	@Override
	public void check(T modified, T attribute, Serializable value) throws ConstraintViolationException {
		for (T instance : modified.getMeta().getSubInstances())
			if (Objects.equals(instance.getValue(), modified.getValue()) && !instance.equals(modified))
				throw new UniqueValueConstraintViolationException("Duplicate value : " + instance.getValue());
	}
}
