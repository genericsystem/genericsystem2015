package org.genericsystem.extendedcarcolor.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;

@SystemGeneric
@Components({ Vehicle.class, Energy.class })
@SingularConstraint
@Dependencies({ Vehicle.class, Energy.class })
public class VehicleEnergy {

}