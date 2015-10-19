package org.genericsystem.admin.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;

/**
 * @author Nicolas Feybesse
 *
 */
@SystemGeneric
@Components(Car.class)
@SingularConstraint
@InstanceValueClassConstraint(Integer.class)
@StringValue("Power")
public class Power {

}
