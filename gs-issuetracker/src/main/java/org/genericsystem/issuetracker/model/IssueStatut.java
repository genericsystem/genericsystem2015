package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.RequiredConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.issuetracker.model.Statut.Open;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@Components({ Issue.class, Statut.class })
@SingularConstraint
@RequiredConstraint
public class IssueStatut implements Generic {

	@SystemGeneric
	@Meta(IssueStatut.class)
	@StringValue("defaultIssueStatut")
	@Components({ Issue.class, Open.class })
	public static class DefaultIssueStatut {
	}

}
