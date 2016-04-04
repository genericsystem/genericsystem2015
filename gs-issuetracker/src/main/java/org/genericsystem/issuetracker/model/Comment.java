package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.common.Generic;

@SystemGeneric
@Components(Issue.class)
@InstanceValueClassConstraint(String.class)
public class Comment implements Generic {

}
