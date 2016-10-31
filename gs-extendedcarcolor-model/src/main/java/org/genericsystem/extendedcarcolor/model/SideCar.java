package org.genericsystem.extendedcarcolor.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;

@SystemGeneric
@Components(Bike.class)
@PropertyConstraint
@InstanceValueClassConstraint(Boolean.class)
public class SideCar {

}