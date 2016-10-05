package org.genericsystem.todomvc;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.constraints.UniqueValueConstraint;

@UniqueValueConstraint
@SystemGeneric
public class Todos {
	@SystemGeneric
	@InstanceValueClassConstraint(Boolean.class)
	// @PropertyConstraint
	@SingularConstraint
	@Components(Todos.class)
	public static class Completed {

	}
}
