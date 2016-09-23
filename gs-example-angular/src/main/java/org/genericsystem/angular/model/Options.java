package org.genericsystem.angular.model;

import org.genericsystem.angular.annotation.Table;
import org.genericsystem.angular.model.Options.AirBag;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.StringValue;

@SystemGeneric
@StringValue("options")
@Dependencies(AirBag.class)
@Table("options")
public class Options {

	@SystemGeneric
	@Meta(Options.class)
	@StringValue("AirBag")
	public static class AirBag {
	}
}
