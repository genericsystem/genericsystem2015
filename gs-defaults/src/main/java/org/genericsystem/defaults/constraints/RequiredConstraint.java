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
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint.AxedCheckableConstraint;
import org.genericsystem.defaults.exceptions.RequiredConstraintViolationException;

/**
 * Represents the constraint to force an attribute to have at least one value.
 *
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of DefaultVertex.
 */
@SystemGeneric
@Meta(MetaAttribute.class)
@Supers(SystemMap.class)
@Components({MetaAttribute.class,DefaultRoot.class})
@InstanceValueClassConstraint(Boolean.class)
@PropertyConstraint
public class RequiredConstraint<T extends DefaultVertex<T>> implements AxedCheckableConstraint<T> {
	@Override
	public void check(T modified, T attribute, Serializable value, int axe, boolean isRevert) throws ConstraintViolationException {
		T base = isRevert ? modified : modified.getComponents().get(axe);
		if (base.isAlive() && base.getHolders(attribute).isEmpty())
			throw new RequiredConstraintViolationException(attribute + " is required for : " + base);
	}

	@Override
	public boolean isCheckable(T modified, boolean isOnAdd, boolean isFlushTime, boolean isRevert) {
		return isRevert == isOnAdd && isFlushTime;
	}
}
