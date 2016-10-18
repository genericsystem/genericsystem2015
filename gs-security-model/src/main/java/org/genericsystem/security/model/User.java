package org.genericsystem.security.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.security.model.User.Password;

@SystemGeneric
@Dependencies({ Password.class, UserRole.class })
public class User {

	@SystemGeneric
	@Components(User.class)
	@SingularConstraint
	public static class Password {

	}
}
