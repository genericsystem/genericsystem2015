package org.genericsystem.extendedcarcolor.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;

@SystemGeneric
@Components({ Vehicle.class, Color.class })
@SingularConstraint
@Dependencies({ Vehicle.class, Color.class })
public class VehicleColor {

}