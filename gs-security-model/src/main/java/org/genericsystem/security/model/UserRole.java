package org.genericsystem.security.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.HideValue;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;

@SystemGeneric
@Components({ User.class, Role.class })
@HideValue
public class UserRole implements Generic {

}
