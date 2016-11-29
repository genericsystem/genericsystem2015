package org.genericsystem.quiz.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.security.model.User;

@SystemGeneric
@Components({ Answer.class, User.class })
@PropertyConstraint
@InstanceValueClassConstraint(Boolean.class)
public class UserAnswer {

}
