package org.genericsystem.exampleswing.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;

@SystemGeneric
@Components(Car.class)
@PropertyConstraint
@InstanceValueClassConstraint(Integer.class)
@StringValue("Power")
public class Power {

}
