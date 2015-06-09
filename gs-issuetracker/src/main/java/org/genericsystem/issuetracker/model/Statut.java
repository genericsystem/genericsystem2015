package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@InstanceValueClassConstraint(String.class)
public class Statut implements Generic {

	@SystemGeneric
	@Meta(Statut.class)
	@StringValue("Open")
	public static class Open {
	}

	@SystemGeneric
	@Meta(Statut.class)
	@StringValue("Coding in progress")
	public static class CodingInProgress {
	}

	@SystemGeneric
	@Meta(Statut.class)
	@StringValue("Reopened")
	public static class Reopened {
	}

	@SystemGeneric
	@Meta(Statut.class)
	@StringValue("Resolved")
	public static class Resolved {
	}

	@SystemGeneric
	@Meta(Statut.class)
	@StringValue("Closed")
	public static class Closed {
	}

}
