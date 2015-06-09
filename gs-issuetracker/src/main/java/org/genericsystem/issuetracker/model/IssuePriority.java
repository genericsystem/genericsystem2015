package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@Components({ Issue.class, Priority.class })
@SingularConstraint
// @RequiredConstraint
public class IssuePriority implements Generic {

}
