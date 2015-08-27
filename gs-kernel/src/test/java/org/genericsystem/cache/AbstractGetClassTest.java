package org.genericsystem.cache;

import org.genericsystem.cache.AnnotationTest.Car;
import org.genericsystem.cache.AnnotationTest.Children;
import org.genericsystem.cache.AnnotationTest.ChildrenGames;
import org.genericsystem.cache.AnnotationTest.Couleur;
import org.genericsystem.cache.AnnotationTest.ElectrikPower;
import org.genericsystem.cache.AnnotationTest.Games;
import org.genericsystem.cache.AnnotationTest.GraphicComposite;
import org.genericsystem.cache.AnnotationTest.Human;
import org.genericsystem.cache.AnnotationTest.HumanPossessCar;
import org.genericsystem.cache.AnnotationTest.HumanPossessVehicle;
import org.genericsystem.cache.AnnotationTest.HumanPossessVehicleTime;
import org.genericsystem.cache.AnnotationTest.Man;
import org.genericsystem.cache.AnnotationTest.ManPossessCar;
import org.genericsystem.cache.AnnotationTest.MyAudi;
import org.genericsystem.cache.AnnotationTest.MyBmw;
import org.genericsystem.cache.AnnotationTest.MyChildren;
import org.genericsystem.cache.AnnotationTest.MyChildrenGames;
import org.genericsystem.cache.AnnotationTest.MyGames;
import org.genericsystem.cache.AnnotationTest.MyGames2;
import org.genericsystem.cache.AnnotationTest.MyMercedes;
import org.genericsystem.cache.AnnotationTest.MySelectableWindow;
import org.genericsystem.cache.AnnotationTest.MyTransformer;
import org.genericsystem.cache.AnnotationTest.MyTransformerChildrenGames;
import org.genericsystem.cache.AnnotationTest.MyVehicle;
import org.genericsystem.cache.AnnotationTest.Myck;
import org.genericsystem.cache.AnnotationTest.OtherVehicle;
import org.genericsystem.cache.AnnotationTest.OtherVehicleType;
import org.genericsystem.cache.AnnotationTest.Power;
import org.genericsystem.cache.AnnotationTest.Puissance;
import org.genericsystem.cache.AnnotationTest.Selectable;
import org.genericsystem.cache.AnnotationTest.SelectableWindow;
import org.genericsystem.cache.AnnotationTest.Selected;
import org.genericsystem.cache.AnnotationTest.Size;
import org.genericsystem.cache.AnnotationTest.Time;
import org.genericsystem.cache.AnnotationTest.Transformer;
import org.genericsystem.cache.AnnotationTest.TransformerChildrenGames;
import org.genericsystem.cache.AnnotationTest.Unit;
import org.genericsystem.cache.AnnotationTest.V123;
import org.genericsystem.cache.AnnotationTest.Vehicle;
import org.genericsystem.cache.AnnotationTest.VehicleType;
import org.genericsystem.cache.AnnotationTest.Voiture;
import org.genericsystem.cache.AnnotationTest.Window;
import org.genericsystem.cache.AnnotationTest.myCar;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGetClassTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(Vehicle.class, MyAudi.class, MyBmw.class, MyMercedes.class, VehicleType.class, OtherVehicleType.class, HumanPossessVehicleTime.class, ManPossessCar.class,
				HumanPossessCar.class, HumanPossessVehicle.class, Time.class, Myck.class, Man.class, Human.class, Unit.class, ElectrikPower.class, myCar.class, Car.class, V123.class, Voiture.class, Couleur.class, Puissance.class, Power.class,
				MyVehicle.class, OtherVehicle.class, MySelectableWindow.class, SelectableWindow.class, Selected.class, Selectable.class, Window.class, Size.class, GraphicComposite.class, MyTransformerChildrenGames.class, TransformerChildrenGames.class,
				MyTransformer.class, Transformer.class, MyChildrenGames.class, ChildrenGames.class, MyChildren.class, Children.class, MyGames2.class, MyGames.class, Games.class);
	}

}
