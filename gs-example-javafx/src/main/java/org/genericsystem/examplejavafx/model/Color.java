package org.genericsystem.examplejavafx.model;

import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.examplejavafx.model.Color.Blue;
import org.genericsystem.examplejavafx.model.Color.Red;
import org.genericsystem.examplejavafx.model.Color.White;
import org.genericsystem.examplejavafx.model.Color.Yellow;


/**
 * @author Nicolas Feybesse
 *
 */
@SystemGeneric
@Dependencies({ White.class, Red.class, Blue.class, Yellow.class })
@StringValue("Color")
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
