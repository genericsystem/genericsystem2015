package org.genericsystem.todomvc;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.UniqueValueConstraint;

@UniqueValueConstraint
@SystemGeneric
public class Todos {
	@SystemGeneric
	@InstanceValueClassConstraint(Boolean.class)
	@PropertyConstraint
	@Components(Todos.class)
	public static class Completed {

	}
}
