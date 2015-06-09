package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator;
import org.genericsystem.defaults.IntSequenceGenerator.StringSequenceGenerator;
import org.genericsystem.mutability.Generic;

@SystemGeneric
@InstanceValueGenerator(StringSequenceGenerator.class)
public class Issue implements Generic {

}
