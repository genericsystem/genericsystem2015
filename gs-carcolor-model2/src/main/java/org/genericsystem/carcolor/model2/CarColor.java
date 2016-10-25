package org.genericsystem.carcolor.model2;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;

@SystemGeneric
@Components({ Car.class, Color.class })
@SingularConstraint
public class CarColor {

}
