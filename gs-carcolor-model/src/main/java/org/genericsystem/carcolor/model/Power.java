package org.genericsystem.carcolor.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;

@SystemGeneric
@Components(Car.class)
@PropertyConstraint
/* @InstanceValueClassConstraint(Integer.class) */
public class Power {

}
