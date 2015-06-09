package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@InstanceValueClassConstraint(String.class)
public class Version implements Generic {

	@SystemGeneric
	@Meta(Version.class)
	@StringValue("GenericSystem 3.0")
	public static class GenericSystem30 {
	}

	@SystemGeneric
	@Meta(Version.class)
	@StringValue("GenericSystem 3.1")
	public static class GenericSystem31 {
	}

	@SystemGeneric
	@Meta(Version.class)
	@StringValue("GenericSystem 3.2")
	public static class GenericSystem32 {
	}
}
