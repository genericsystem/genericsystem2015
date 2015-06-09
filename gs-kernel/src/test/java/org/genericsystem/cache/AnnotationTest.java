package org.genericsystem.cache;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.constraints.UniqueValueConstraint;
import org.genericsystem.api.core.annotations.value.IntValue;
import org.testng.annotations.Test;

@Test
public class AnnotationTest extends AbstractTest {

	public void test001_Generic() {
		Engine engine = new Engine(Vehicle.class, Human.class, Myck.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic myck = engine.find(Myck.class);
		assert vehicle.isStructural();
		assert human.isStructural();
		assert myck.isConcrete();
	}

	public void test001_remove() {
		Engine engine = new Engine(Vehicle.class);
		Generic vehicle = engine.find(Vehicle.class);
		catchAndCheckCause(() -> vehicle.remove(), IllegalAccessException.class);
	}

	public void test002_remove() {
		Engine engine = new Engine(OtherVehicle.class);
		Generic vehicle = engine.find(OtherVehicle.class);
		catchAndCheckCause(() -> vehicle.remove(), IllegalAccessException.class);
	}

	public void test001_instanceof() {
		Engine engine = new Engine(Vehicle.class);
		assert engine.find(Vehicle.class) instanceof Vehicle : Vehicle.class.isInterface() + " " + engine.find(Vehicle.class).getClass();
		assert engine.getInstance(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info() + "   " + engine.getInstance(Vehicle.class).info();
		assert engine.getInstances().stream().anyMatch(x -> x instanceof Vehicle);
	}

	public void test002_instanceof() {
		Engine engine = new Engine(VehicleType.class);
		assert engine.find(VehicleType.class) instanceof VehicleType;
		VehicleType vehicle = engine.find(VehicleType.class);
		assert vehicle.addInstance("myBmw") instanceof VehicleInstance;
		assert vehicle.setInstance("myBmw") instanceof VehicleInstance;
		VehicleInstance vi = (VehicleInstance) vehicle.setInstance("myBmw");
	}

	public void test0022_instanceof() {
		Engine engine = new Engine(OtherVehicleType.class);
		Generic vehicle = engine.find(OtherVehicleType.class);
		assert vehicle.addInstance("myBmw") instanceof VehicleInstance;
		assert vehicle.setInstance("myBmw") instanceof VehicleInstance;
		VehicleInstance vi = (VehicleInstance) vehicle.setInstance("myBmw");
	}

	public void test002_instanceof_getInstances() {
		Engine engine = new Engine(VehicleType.class);
		VehicleType vehicle = engine.find(VehicleType.class);
		assert vehicle.addInstance("myBmw") instanceof VehicleInstance;
		assert vehicle.getInstances().stream().allMatch(x -> x instanceof VehicleInstance);
	}

	public void test003_instanceof() {
		Engine engine = new Engine(MyAudi.class);
		assert engine.find(MyAudi.class) instanceof VehicleInstance : engine.find(MyAudi.class).getClass();
		assert engine.find(MyAudi.class) instanceof MyAudi : engine.find(MyAudi.class).getClass();
	}

	public void test004_instanceof() {

		catchAndCheckCause(() -> new Engine(MyBmw.class), InstantiationException.class);
		catchAndCheckCause(() -> new Engine(MyMercedes.class), InstantiationException.class);
	}

	public static class VehicleInstance implements Generic {

	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static class MyAudi extends VehicleInstance {
	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static class MyBmw implements Generic {
	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static class MyMercedes {
	}

	@SystemGeneric
	@InstanceClass(VehicleInstance.class)
	public static class VehicleType implements Generic {

	}

	@SystemGeneric
	@InstanceClass(VehicleInstance.class)
	public static class OtherVehicleType {

	}

	public void test002_SuperGeneric() {
		Engine engine = new Engine(Vehicle.class, Car.class, myCar.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic car = engine.find(Car.class);
		Generic myCar = engine.find(myCar.class);

		assert vehicle.isStructural();
		assert car.isStructural();
		assert vehicle.getSupers().size() == 0 : vehicle.getSupers();
		assert car.getSupers().size() == 1 : car.getSupers();
		assert car.getSupers().contains(vehicle);
		assert myCar.getSupers().size() == 0 : myCar.getSupers();
		assert myCar.getMeta().equals(car);
	}

	public void test003_Attribute() {
		Engine engine = new Engine(Vehicle.class, Power.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic power = engine.find(Power.class);
		assert power.isStructural();
		assert vehicle.getAttributes(engine).contains(power) : vehicle.getAttributes(engine);
	}

	public void test004_AttributeValue() {
		Engine engine = new Engine(V123.class);
		Generic myVehicle = engine.find(MyVehicle.class);
		engine.find(V123.class);
		assert myVehicle.getValues(engine.find(Power.class)).size() == 1;
		assert myVehicle.getValues(engine.find(Power.class)).contains(new Integer(123)) : myVehicle.getValues(engine.find(Power.class));
	}

	public void test005_SuperAttribute() {
		Engine engine = new Engine(Car.class, ElectrikPower.class);
		Generic car = engine.find(Car.class);
		Generic electrikPowerCar = engine.find(ElectrikPower.class);
		assert car.getAttributes(engine).contains(electrikPowerCar) : car.getAttributes(engine);
	}

	public void test006_AttributeOnAttribute() {
		Engine engine = new Engine(ElectrikPower.class, Unit.class);
		Generic electrikPowerCar = engine.find(ElectrikPower.class);
		Generic unit = engine.find(Unit.class);
		assert unit.isCompositeOf(electrikPowerCar);
		assert unit.isStructural();
		assert electrikPowerCar.getAttributes(engine).contains(unit);
	}

	public void test007_Relation() {
		Engine engine = new Engine(Vehicle.class, Human.class, HumanPossessVehicle.class);
		engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic possess = engine.find(HumanPossessVehicle.class);
		assert human.getAttributes().contains(possess);
	}

	public void test008_SubRelation() {
		Engine engine = new Engine(Car.class, Human.class, HumanPossessVehicle.class, HumanPossessCar.class);
		engine.find(Car.class);
		Generic human = engine.find(Human.class);
		Generic possessVehicle = engine.find(HumanPossessVehicle.class);
		Generic possessCar = engine.find(HumanPossessCar.class);
		assert possessCar.inheritsFrom(possessVehicle);
		assert human.getAttributes().contains(possessCar) : human.getAttributes();

	}

	public void test009_SymetricSuperRelation() {
		Engine engine = new Engine(Car.class, Human.class, Man.class, HumanPossessVehicle.class, ManPossessCar.class);
		engine.find(Car.class);
		Generic human = engine.find(Human.class);
		Generic man = engine.find(Man.class);
		Generic humanPossessVehicle = engine.find(HumanPossessVehicle.class);
		Generic manPossessCar = engine.find(ManPossessCar.class);
		assert human.getAttributes().contains(humanPossessVehicle);
		assert man.getAttributes().contains(manPossessCar) : man.getAttributes();
		assert manPossessCar.inheritsFrom(humanPossessVehicle);
	}

	public void test010_TernaryRelation() {
		Engine engine = new Engine(Vehicle.class, Human.class, Time.class, HumanPossessVehicleTime.class);
		engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		engine.find(Time.class);
		Generic possess = engine.find(HumanPossessVehicleTime.class);
		assert human.getAttributes().contains(possess);
	}

	public void test011_getDirectSubGenericsWithDiamondProblem() {
		Engine engine = new Engine(GraphicComposite.class, Window.class, Selectable.class, SelectableWindow.class);
		Generic graphicComposite = engine.find(GraphicComposite.class);
		Generic window = engine.find(Window.class);
		Generic selectable = engine.find(Selectable.class);
		Generic selectableWindow = engine.find(SelectableWindow.class);

		assert selectableWindow.getSupers().size() == 2 : selectableWindow.getSupers();
		assert selectableWindow.getSupers().contains(selectable) : selectableWindow.getSupers();
		assert selectableWindow.getSupers().contains(window) : selectableWindow.getSupers();

		assert window.getSupers().size() == 1 : window.getSupers();
		assert window.getSupers().contains(graphicComposite) : window.getSupers();

		assert selectable.getSupers().size() == 1 : selectable.getSupers();
		assert selectable.getSupers().contains(graphicComposite) : selectable.getSupers();

		assert selectableWindow.getSupers().size() == 2;
		assert selectableWindow.getSupers().contains(selectable);
		assert selectableWindow.getSupers().contains(window);

		assert selectableWindow.inheritsFrom(selectable);
		assert selectableWindow.inheritsFrom(window);
		assert selectableWindow.inheritsFrom(graphicComposite);
	}

	public void test012_Value() {
		Engine engine = new Engine(SelectableWindow.class, Size.class, Selected.class, MySelectableWindow.class);
		Generic selectableWindow = engine.find(SelectableWindow.class);
		Generic size = engine.find(Size.class);
		Generic selectedSelectable = engine.find(Selected.class);
		Generic mySelectableWindow = engine.find(MySelectableWindow.class);
		assert mySelectableWindow.isInstanceOf(selectableWindow) : mySelectableWindow.info() + selectableWindow.info();

		assert engine.find(Selectable.class).isAncestorOf(mySelectableWindow);
		Generic vTrue = selectedSelectable.addInstance(true, selectedSelectable.getComponents().toArray(new Generic[1]));
		Generic v12 = size.addInstance(12, size.getComponents().toArray(new Generic[1]));

		assert selectableWindow.getInstances().size() == 1 : selectableWindow.getInstances();
		assert selectableWindow.getInstances().contains(mySelectableWindow);
		assert mySelectableWindow.getHolders(size).size() == 1 : mySelectableWindow.getHolders(size);
		assert mySelectableWindow.getHolders(size).contains(v12) : mySelectableWindow.getHolders(size);
		assert mySelectableWindow.getHolders(selectedSelectable).size() == 1;
		assert mySelectableWindow.getHolders(selectedSelectable).contains(vTrue);
	}

	public void test013_MultiInheritanceComplexStructural() {
		Engine engine = new Engine(Games.class, Children.class, Vehicle.class, Human.class, ChildrenGames.class, Transformer.class, TransformerChildrenGames.class);
		Generic games = engine.find(Games.class);
		Generic children = engine.find(Children.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic childrenGames = engine.find(ChildrenGames.class);
		Generic transformer = engine.find(Transformer.class);
		Generic transformerChildrenGames = engine.find(TransformerChildrenGames.class);

		assert transformerChildrenGames.inheritsFrom(games);
		assert transformerChildrenGames.inheritsFrom(children);
		assert transformerChildrenGames.inheritsFrom(vehicle);
		assert transformerChildrenGames.inheritsFrom(human);

		assert transformerChildrenGames.inheritsFrom(childrenGames);
		assert transformerChildrenGames.getSupers().contains(childrenGames) : transformerChildrenGames.info();
		assert transformerChildrenGames.getSupers().contains(transformer);
		assert transformerChildrenGames.getInheritings().size() == 0;
		assert transformerChildrenGames.getComposites().size() == 0;

		assert childrenGames.getSupers().contains(games);
		assert childrenGames.getSupers().contains(children);
		assert childrenGames.getInheritings().contains(transformerChildrenGames);
		assert childrenGames.getComposites().size() == 0;

		assert transformer.getSupers().contains(vehicle);
		assert transformer.getSupers().contains(human);
		assert transformer.getInheritings().contains(transformerChildrenGames);
		assert transformer.getComposites().size() == 0;
	}

	public void test014_MultiInheritanceComplexValue() {
		Engine engine = new Engine(MyGames.class, MyChildren.class, MyVehicle.class, Myck.class, MyChildrenGames.class, ChildrenGames.class, MyTransformer.class, Transformer.class, TransformerChildrenGames.class, MyTransformerChildrenGames.class);
		Generic myGames = engine.find(MyGames.class);
		Generic myChildren = engine.find(MyChildren.class);
		Generic myVehicle = engine.find(MyVehicle.class);
		Generic myck = engine.find(Myck.class);
		Generic myChildrenGames = engine.find(MyChildrenGames.class);
		Generic childrenGames = engine.find(ChildrenGames.class);
		Generic myTransformer = engine.find(MyTransformer.class);
		Generic transformer = engine.find(Transformer.class);
		Generic transformerChildrenGames = engine.find(TransformerChildrenGames.class);
		Generic myTransformerChildrenGames = engine.find(MyTransformerChildrenGames.class);

		assert myTransformerChildrenGames.isInstanceOf(transformerChildrenGames) : myTransformerChildrenGames.info() + transformerChildrenGames.info();

		assert !myTransformerChildrenGames.inheritsFrom(myGames);
		assert !myTransformerChildrenGames.inheritsFrom(myChildren);
		assert !myTransformerChildrenGames.inheritsFrom(myVehicle);
		assert !myTransformerChildrenGames.inheritsFrom(myck);
		assert !myTransformerChildrenGames.inheritsFrom(myChildrenGames);
		assert !myTransformerChildrenGames.inheritsFrom(myTransformer);
		assert myTransformerChildrenGames.getSupers().size() == 0;
		assert myTransformerChildrenGames.getInheritings().size() == 0;
		assert myTransformerChildrenGames.getComposites().size() == 0;

		assert transformer.getSupers().size() == 2;
		assert transformer.getSupers().contains(engine.find(Human.class));
		assert transformer.getSupers().contains(engine.find(Vehicle.class));

		assert transformerChildrenGames.getInstances().contains(myTransformerChildrenGames);
		assert myTransformerChildrenGames.isInstanceOf(transformerChildrenGames);

		assert transformerChildrenGames.getSupers().size() == 2;
		assert transformerChildrenGames.getSupers().contains(transformer);
		assert transformerChildrenGames.getSupers().contains(childrenGames);

		assert !myChildrenGames.inheritsFrom(myGames);
		assert !myChildrenGames.inheritsFrom(myChildren);
		assert myChildrenGames.getSupers().size() == 0;// .contains(childrenGames);
		assert myChildrenGames.getInheritings().size() == 0;
		assert myChildrenGames.getComposites().size() == 0;

		assert childrenGames.getSupers().size() == 2;
		assert childrenGames.getSupers().contains(engine.find(Games.class));
		assert childrenGames.getSupers().contains(engine.find(Children.class));

		assert childrenGames.getInstances().contains(myChildrenGames);
		assert myChildrenGames.isInstanceOf(childrenGames);

		assert !myTransformer.inheritsFrom(myVehicle);
		assert !myTransformer.inheritsFrom(myck);
		assert myTransformer.getSupers().size() == 0;// .contains(transformer);
		assert myTransformer.getInheritings().size() == 0;
		assert myTransformer.getComposites().size() == 0;

		assert transformer.getInstances().contains(myTransformer);
		assert myTransformer.isInstanceOf(transformer);
	}

	public void test015_propertyConstraint() {
		Engine engine = new Engine(Vehicle.class, Puissance.class);
		Generic voiture = engine.find(Vehicle.class);
		Generic puissance = engine.find(Puissance.class);

		assert puissance.isPropertyConstraintEnabled();
	}

	// public void test016_requiredConstraint() {
	// Engine engine = new Engine(Vehicle.class, Puissance.class);
	// Generic voiture = engine.find(Vehicle.class);
	// Generic puissance = engine.find(Puissance.class);
	//
	// assert puissance.isRequiredConstraintEnabled(Statics.NO_POSITION);
	// }

	public void test017_singularConstraint() {
		Engine engine = new Engine(Vehicle.class, Puissance.class);
		Generic voiture = engine.find(Vehicle.class);
		Generic puissance = engine.find(Puissance.class);

		assert puissance.isSingularConstraintEnabled(0);
	}

	public void test018_uniqueValueConstraint() {
		Engine engine = new Engine(Vehicle.class, Puissance.class);
		Generic voiture = engine.find(Vehicle.class);
		Generic puissance = engine.find(Puissance.class);

		assert puissance.isUniqueValueEnabled();
	}

	public void test019_uniqueClassConstraint() {
		Engine engine = new Engine(Vehicle.class, Puissance.class);
		Generic voiture = engine.find(Vehicle.class);
		Generic puissance = engine.find(Puissance.class);

		assert puissance.getInstanceValueClassConstraint().equals(Integer.class);
	}

	public void test020_dependencies() {
		Engine engine = new Engine(Voiture.class);
		Generic puissance = engine.find(Puissance.class);
		Generic couleur = engine.find(Couleur.class);
		assert puissance instanceof Puissance;
		assert couleur instanceof Couleur;
	}

	@SystemGeneric
	public static class Games implements Generic {
	}

	@SystemGeneric
	@Meta(Games.class)
	public static class MyGames implements Generic {
	}

	@SystemGeneric
	@Meta(Games.class)
	public static class MyGames2 implements Generic {
	}

	@SystemGeneric
	public static class Children implements Generic {
	}

	@SystemGeneric
	@Meta(Children.class)
	public static class MyChildren implements Generic {
	}

	@SystemGeneric
	@Supers({ Games.class, Children.class })
	public static class ChildrenGames implements Generic {
	}

	@SystemGeneric
	@Meta(ChildrenGames.class)
	public static class MyChildrenGames implements Generic {
	}

	@SystemGeneric
	@Supers({ Human.class, Vehicle.class })
	public static class Transformer implements Generic {
	}

	@SystemGeneric
	@Meta(Transformer.class)
	public static class MyTransformer implements Generic {
	}

	@SystemGeneric
	@Supers({ Transformer.class, ChildrenGames.class })
	public static class TransformerChildrenGames implements Generic {
	}

	@SystemGeneric
	@Meta(TransformerChildrenGames.class)
	public static class MyTransformerChildrenGames implements Generic {
	}

	@SystemGeneric
	public static class GraphicComposite implements Generic {

	}

	@SystemGeneric
	@Components(GraphicComposite.class)
	public static class Size implements Generic {

	}

	@SystemGeneric
	@Supers(GraphicComposite.class)
	public static class Window extends GraphicComposite {

	}

	@SystemGeneric
	@Supers(GraphicComposite.class)
	public static class Selectable implements Generic {

	}

	@SystemGeneric
	@Components(Selectable.class)
	public static class Selected implements Generic {

	}

	@SystemGeneric
	@Supers({ Selectable.class, Window.class })
	public static class SelectableWindow implements Generic {

	}

	@SystemGeneric
	@Meta(SelectableWindow.class)
	public static class MySelectableWindow implements Generic {

	}

	@SystemGeneric
	public static class Vehicle implements Generic {

	}

	@SystemGeneric
	public static class OtherVehicle {

	}

	@SystemGeneric
	@Meta(Vehicle.class)
	public static class MyVehicle implements Generic {
	}

	@SystemGeneric
	@Components(Vehicle.class)
	public static class Power implements Generic {

	}

	@SystemGeneric
	@Components(Vehicle.class)
	@PropertyConstraint
	@SingularConstraint(ApiStatics.BASE_POSITION)
	// @RequiredConstraint
	@UniqueValueConstraint
	@InstanceValueClassConstraint(Integer.class)
	@Dependencies(Couleur.class)
	public static class Puissance implements Generic {

	}

	public static class Couleur implements Generic {

	}

	@Dependencies(Puissance.class)
	public static class Voiture implements Generic {

	}

	@SystemGeneric
	@Meta(Power.class)
	@Components(MyVehicle.class)
	@IntValue(123)
	public static class V123 implements Generic {

	}

	@SystemGeneric
	@Supers(Vehicle.class)
	public static class Car implements Generic {

	}

	@SystemGeneric
	@Meta(Car.class)
	public static class myCar implements Generic {
	}

	@SystemGeneric
	@Components(Car.class)
	@Supers(Power.class)
	public static class ElectrikPower implements Generic {

	}

	@SystemGeneric
	@Components(ElectrikPower.class)
	public static class Unit implements Generic {

	}

	@SystemGeneric
	public static class Human implements Generic {
	}

	@SystemGeneric
	public static class Man extends Human {
	}

	@SystemGeneric
	@Meta(Human.class)
	public static class Myck implements Generic {
	}

	@SystemGeneric
	public static class Time implements Generic {
	}

	@SystemGeneric
	@Components({ Human.class, Vehicle.class })
	public static class HumanPossessVehicle implements Generic {
	}

	@SystemGeneric
	@Components({ Human.class, Car.class })
	@Supers(HumanPossessVehicle.class)
	public static class HumanPossessCar extends HumanPossessVehicle {
	}

	@SystemGeneric
	@Components({ Man.class, Car.class })
	@Supers(HumanPossessVehicle.class)
	public static class ManPossessCar extends HumanPossessVehicle {
	}

	@SystemGeneric
	@Components({ Human.class, Vehicle.class, Time.class })
	public static class HumanPossessVehicleTime implements Generic {
	}

}
