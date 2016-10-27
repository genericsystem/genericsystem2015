package org.genericsystem.carcolor.model.extended;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;

@SystemGeneric
@Components({ Vehicle.class, VehicleEngine.class })
@SingularConstraint
@Dependencies({ Vehicle.class, VehicleEngine.class })
public class VehicleVehicleEngine {

}