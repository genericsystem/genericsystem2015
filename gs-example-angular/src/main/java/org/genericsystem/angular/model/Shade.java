package org.genericsystem.angular.model;

import org.genericsystem.angular.annotation.Column;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.StringValue;

@SystemGeneric
@Components(Color.class)
@StringValue("shade")
@Column("shade")
public class Shade {

}
