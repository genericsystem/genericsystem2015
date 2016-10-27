package org.genericsystem.carcolor.model.extended;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;

@SystemGeneric
@Components(Vehicle.class)
@PropertyConstraint
@InstanceValueClassConstraint(Integer.class)
public class Mileage {

}