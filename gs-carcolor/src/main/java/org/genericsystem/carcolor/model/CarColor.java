package org.genericsystem.carcolor.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.HideValue;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;

@SystemGeneric
@Components({ Car.class, Color.class })
@SingularConstraint
@HideValue
public class CarColor {

}
