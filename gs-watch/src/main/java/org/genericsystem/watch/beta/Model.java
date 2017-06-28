package org.genericsystem.watch.beta;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;

public class Model {

	@SystemGeneric
	public static class Task {
	}

	@SystemGeneric
	@InstanceValueClassConstraint(String.class)
	public static class Message {
	}

}