package org.genericsystem.defaults.constraints;

import java.io.Serializable;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint.AxedCheckedConstraint;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;

/**
 * Represents the constraint to allow only one value for a relation.
 *
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of DefaultVertex.
 */
@SystemGeneric
@Meta(MetaAttribute.class)
@Supers(SystemMap.class)
@Components(MetaAttribute.class)
@InstanceValueClassConstraint(Boolean.class)
@PropertyConstraint
public class SingularConstraint<T extends DefaultVertex<T>> implements AxedCheckedConstraint<T> {
	@Override
	public void check(T modified, T attribute, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException {
		T base = isRevert ? modified : modified.getComponents().get(axe);
		if (base.getHolders(attribute).size() > 1)
			throw new SingularConstraintViolationException(base + " has more than one link : " + base.getHolders(attribute).info() + " for attribute : " + attribute);
	}
}
