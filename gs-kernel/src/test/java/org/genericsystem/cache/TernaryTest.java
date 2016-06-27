package org.genericsystem.cache;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.cache.TernaryTest.Color.White;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Engine;
import org.testng.annotations.Test;

@Test
public class TernaryTest extends AbstractTest {

	
	@SystemGeneric
	public static class Car {

	}
	
	@SystemGeneric
	@Components({ Car.class, Color.class })
	@SingularConstraint
	public static class CarColor {

		@SystemGeneric
		@Meta(CarColor.class)
		@StringValue("DefaultCarColor")
		@Components({ Car.class, White.class })
		public static class DefaultCarColor {
		}

	}
	
	@SystemGeneric
	//@InstanceColorize
	public static class Color {

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
	
	
	public void test000(){
		
		Engine engine = new  Engine(Car.class,Color.class,CarColor.class);
		Generic car = engine.find(Car.class);
		Generic color = engine.find(Color.class);
		Generic carColor = engine.find(CarColor.class);
		carColor.enableSingularConstraint(0);
		Generic person = engine.setInstance("Person");
		Generic category = engine.setInstance("Category");
		Generic carPerson = car.setRelation("CarDriverOwner", category, person);
		engine.getCurrentCache().flush();
	}
	
}
