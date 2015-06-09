package org.genericsystem.admin.model;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;

/**
 * @author Nicolas Feybesse
 *
 */
@SystemGeneric
@StringValue("Car")
@InstanceValueClassConstraint(String.class)
public class Car {

}
