package org.genericsystem.security.model;

import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.common.Generic;
import org.genericsystem.security.model.Role.Admin;

@SystemGeneric
@Dependencies(Admin.class)
public class Role implements Generic {

	@SystemGeneric
	@StringValue("Admin")
	@Meta(Role.class)
	public static class Admin {

	}
}
