package org.genericsystem.distributed;

import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.AnnotationTest.Car;
import org.genericsystem.distributed.AnnotationTest.Children;
import org.genericsystem.distributed.AnnotationTest.ChildrenGames;
import org.genericsystem.distributed.AnnotationTest.Couleur;
import org.genericsystem.distributed.AnnotationTest.ElectrikPower;
import org.genericsystem.distributed.AnnotationTest.Games;
import org.genericsystem.distributed.AnnotationTest.GraphicComposite;
import org.genericsystem.distributed.AnnotationTest.Human;
import org.genericsystem.distributed.AnnotationTest.HumanPossessCar;
import org.genericsystem.distributed.AnnotationTest.HumanPossessVehicle;
import org.genericsystem.distributed.AnnotationTest.HumanPossessVehicleTime;
import org.genericsystem.distributed.AnnotationTest.Man;
import org.genericsystem.distributed.AnnotationTest.ManPossessCar;
import org.genericsystem.distributed.AnnotationTest.MyAudi;
import org.genericsystem.distributed.AnnotationTest.MyBmw;
import org.genericsystem.distributed.AnnotationTest.MyChildren;
import org.genericsystem.distributed.AnnotationTest.MyChildrenGames;
import org.genericsystem.distributed.AnnotationTest.MyGames;
import org.genericsystem.distributed.AnnotationTest.MyGames2;
import org.genericsystem.distributed.AnnotationTest.MyMercedes;
import org.genericsystem.distributed.AnnotationTest.MySelectableWindow;
import org.genericsystem.distributed.AnnotationTest.MyTransformer;
import org.genericsystem.distributed.AnnotationTest.MyTransformerChildrenGames;
import org.genericsystem.distributed.AnnotationTest.MyVehicle;
import org.genericsystem.distributed.AnnotationTest.Myck;
import org.genericsystem.distributed.AnnotationTest.OtherVehicle;
import org.genericsystem.distributed.AnnotationTest.OtherVehicleType;
import org.genericsystem.distributed.AnnotationTest.Power;
import org.genericsystem.distributed.AnnotationTest.Puissance;
import org.genericsystem.distributed.AnnotationTest.Selectable;
import org.genericsystem.distributed.AnnotationTest.SelectableWindow;
import org.genericsystem.distributed.AnnotationTest.Selected;
import org.genericsystem.distributed.AnnotationTest.Size;
import org.genericsystem.distributed.AnnotationTest.Time;
import org.genericsystem.distributed.AnnotationTest.Transformer;
import org.genericsystem.distributed.AnnotationTest.TransformerChildrenGames;
import org.genericsystem.distributed.AnnotationTest.Unit;
import org.genericsystem.distributed.AnnotationTest.V123;
import org.genericsystem.distributed.AnnotationTest.Vehicle;
import org.genericsystem.distributed.AnnotationTest.VehicleType;
import org.genericsystem.distributed.AnnotationTest.Voiture;
import org.genericsystem.distributed.AnnotationTest.Window;
import org.genericsystem.distributed.AnnotationTest.myCar;
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
