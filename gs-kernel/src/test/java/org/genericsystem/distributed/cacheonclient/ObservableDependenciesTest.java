package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Car;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Children;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.ChildrenGames;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Couleur;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.ElectrikPower;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Games;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.GraphicComposite;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Human;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.HumanPossessCar;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.HumanPossessVehicle;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.HumanPossessVehicleTime;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Man;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.ManPossessCar;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyAudi;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyBmw;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyChildren;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyChildrenGames;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyGames;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyGames2;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyMercedes;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MySelectableWindow;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyTransformer;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyTransformerChildrenGames;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.MyVehicle;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Myck;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.OtherVehicle;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.OtherVehicleType;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Power;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Puissance;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Selectable;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.SelectableWindow;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Selected;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Size;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Time;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Transformer;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.TransformerChildrenGames;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Unit;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.V123;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Vehicle;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.VehicleType;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Voiture;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.Window;
import org.genericsystem.distributed.cacheonserver.AnnotationTest.myCar;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class ObservableDependenciesTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(Vehicle.class, MyAudi.class, MyBmw.class, MyMercedes.class, VehicleType.class, OtherVehicleType.class, HumanPossessVehicleTime.class, ManPossessCar.class,
				HumanPossessCar.class, HumanPossessVehicle.class, Time.class, Myck.class, Man.class, Human.class, Unit.class, ElectrikPower.class, myCar.class, Car.class, V123.class, Voiture.class, Couleur.class, Puissance.class, Power.class,
				MyVehicle.class, OtherVehicle.class, MySelectableWindow.class, SelectableWindow.class, Selected.class, Selectable.class, Window.class, Size.class, GraphicComposite.class, MyTransformerChildrenGames.class, TransformerChildrenGames.class,
				MyTransformer.class, Transformer.class, MyChildrenGames.class, ChildrenGames.class, MyChildren.class, Children.class, MyGames2.class, MyGames.class, Games.class);
	}

	public void test01() {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("MyCar");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");
		assert engine.getCurrentCache().getObservableDependencies(car).size() == 5;
	}

	public void test02() throws InterruptedException {
		// possible fail on first assert: connection might be too fast
		HeavyClientEngine engine = new HeavyClientEngine();
		ObservableList<Generic> observableDependencies = engine.getCurrentCache().getObservableDependencies(engine);
		assert observableDependencies.isEmpty();
		Thread.sleep(100);
		assert !observableDependencies.isEmpty();
	}

	public void test03() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine(Vehicle.class);
		Generic vehicle = engine.find(Vehicle.class);
		ObservableList list = engine.getCurrentCache().getObservableDependencies(vehicle);
		Thread.sleep(100);
		assert !list.isEmpty();
	}

	public void test04() throws InterruptedException, ConcurrencyControlException {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("MyCar");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");
		engine.getCurrentCache().tryFlush();
		ObservableList list = engine.getCurrentCache().getObservableDependencies(car);
		Thread.sleep(100);
		assert engine.getCurrentCache().getObservableDependencies(car).size() == 5 : engine.getCurrentCache().getObservableDependencies(car).size();
	}

	public void test05() throws InterruptedException, ConcurrencyControlException {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("MyCar");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");
		ObservableList list = engine.getCurrentCache().getObservableDependencies(car);
		Thread.sleep(100);
		engine.getCurrentCache().shiftTs();
		assert list.size() == 5 : list.size();

	}
}
