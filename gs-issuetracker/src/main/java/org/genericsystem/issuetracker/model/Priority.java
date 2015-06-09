package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@InstanceValueClassConstraint(String.class)
public class Priority implements Generic {

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Blocker")
	public static class Blocker {
	}

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Critical")
	public static class Critical {
	}

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Major")
	public static class Major {
	}

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Minor")
	public static class Minor {
	}

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Optional")
	public static class Optional {
	}

	@SystemGeneric
	@Meta(Priority.class)
	@StringValue("Trivial")
	public static class Trivial {
	}
}
