package org.genericsystem.angular.model;

import org.genericsystem.angular.annotation.Table;
import org.genericsystem.angular.model.Color.Blue;
import org.genericsystem.angular.model.Color.Red;
import org.genericsystem.angular.model.Color.Yellow;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.StringValue;

@SystemGeneric
@Dependencies({ Red.class, Blue.class, Yellow.class })
@StringValue("color")
@Table("color")
public class Color {

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("White")
	public static class White {
	}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Red")
	public static class Red {
	}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Blue")
	public static class Blue {
	}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Yellow")
	public static class Yellow {
	}
}
