package org.genericsystem.issuetracker.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;

@SystemGeneric
@Components({ Issue.class, Version.class })
public class IssueVersion implements Generic {

}
