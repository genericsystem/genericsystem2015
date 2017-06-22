package org.genericsystem.watch.beta;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;


@SystemGeneric
@Components(Task.class)
@PropertyConstraint
@InstanceValueClassConstraint(Integer.class)
public class State {

}
