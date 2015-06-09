package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@Components(Issue.class)
@PropertyConstraint
@InstanceValueClassConstraint(String.class)
public class Description implements Generic {

}
