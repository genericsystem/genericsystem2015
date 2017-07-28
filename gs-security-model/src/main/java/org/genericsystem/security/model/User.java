package org.genericsystem.security.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.HashedInstanceValue;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.common.Generic;
import org.genericsystem.security.model.User.Password;

@SystemGeneric
@Dependencies({ Password.class, UserRole.class })
public class User implements Generic {

	@SystemGeneric
	@Components(User.class)
	@SingularConstraint
	@HashedInstanceValue
	@InstanceValueClassConstraint(byte[].class)
	@Dependencies(Salt.class)
	public static class Password implements Generic {

	}

	@SystemGeneric
	@Components(Password.class)
	@SingularConstraint
	@HashedInstanceValue
	@InstanceValueClassConstraint(byte[].class)
	@StringValue("Salt")
	public static class Salt implements Generic {

	}
}
