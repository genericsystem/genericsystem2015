package org.genericsystem.defaults.constraints;

import java.io.Serializable;
import java.util.stream.Collectors;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.defaults.constraints.Constraint.CheckableConstraint;
import org.genericsystem.defaults.exceptions.PropertyConstraintViolationException;

/**
 * Represents the constraint to allow only one value for an attribute.
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
@org.genericsystem.api.core.annotations.constraints.PropertyConstraint
public class PropertyConstraint<T extends DefaultVertex<T>> implements CheckableConstraint<T> {
	@Override
	public void check(T modified, T attribute, Serializable value) throws ConstraintViolationException {
		T base = modified.getBaseComponent();
		Snapshot<T> snapshot = () -> base.getHolders(attribute).stream().filter(x -> modified.getComponents().equals(x.getComponents()) && modified.getMeta().equals(x.getMeta()));
		if (snapshot.size() > 1)
			throw new PropertyConstraintViolationException("For attribute : " + attribute + " these holders violates property constraint : \n" + snapshot.stream().map(x -> x.info() + "\n").collect(Collectors.toList()));
	}

	@Override
	public boolean isCheckable(T modified, boolean isOnAdd, boolean isFlushTime, boolean isRevert) {
		return isOnAdd || (modified.getValue() == null);
	}
}
