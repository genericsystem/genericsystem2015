package org.genericsystem.quiz.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.security.model.User;

@SystemGeneric
@Components({ User.class, Quiz.class })
@PropertyConstraint
public class ScoreUserQuiz {

}
