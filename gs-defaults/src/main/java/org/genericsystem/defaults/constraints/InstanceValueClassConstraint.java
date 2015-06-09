package org.genericsystem.defaults.constraints;

import java.io.Serializable;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint.CheckedConstraint;
import org.genericsystem.defaults.exceptions.InstanceValueClassConstraintViolationException;

/**
 * Represents the constraint to precise the <code>Class</code> of the value of instances.
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
@PropertyConstraint
@org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint(Class.class)
public class InstanceValueClassConstraint<T extends DefaultVertex<T>> implements CheckedConstraint<T> {
	@SuppressWarnings("unchecked")
	@Override
	public void check(T modified, T attribute, Serializable value) throws ConstraintViolationException {
		if (modified.getValue() != null && !((Class<? extends Serializable>) value).isAssignableFrom(modified.getValue().getClass()))
			throw new InstanceValueClassConstraintViolationException(modified.info() + " should be " + modified.getInstanceValueClassConstraint());
	}
}
