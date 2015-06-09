package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.RequiredConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.constraints.UniqueValueConstraint;
import org.genericsystem.api.core.annotations.value.IntValue;
import org.testng.annotations.Test;

@Test
public class AnnotationTest extends AbstractTest {

	public void test001() {
		Root engine = new Root(Vehicle.class, Human.class, Myck.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic myck = engine.find(Myck.class);
		assert vehicle.isStructural();
		assert human.isStructural();
		assert myck.isConcrete();
	}

	public void test002() {
		Root engine = new Root(Vehicle.class);
		Generic vehicle = engine.find(Vehicle.class);
		catchAndCheckCause(() -> vehicle.remove(), IllegalAccessException.class);
	}

	public void test003() {
		Root engine = new Root(OtherVehicle.class);
		Generic vehicle = engine.find(OtherVehicle.class);
		catchAndCheckCause(() -> vehicle.remove(), IllegalAccessException.class);
	}

	public void test004() {
		Root engine = new Root(Vehicle.class);
		assert engine.find(Vehicle.class) instanceof Vehicle;
		assert engine.getInstance(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info() + "   " + engine.getInstance(Vehicle.class).info();
		assert engine.getInstances().stream().anyMatch(x -> x instanceof Vehicle);
	}

	public void test005() {
		Root engine = new Root(VehicleType.class);
		assert engine.find(VehicleType.class) instanceof VehicleType;
		VehicleType vehicle = engine.find(VehicleType.class);
		assert vehicle.addInstance("myBmw") instanceof VehicleInstance;
		assert vehicle.setInstance("myBmw") instanceof VehicleInstance;
	}

	public void test006() {
		Root engine = new Root(OtherVehicleType.class);
		Generic vehicle = engine.find(OtherVehicleType.class);
		assert vehicle.addInstance("myBmw") instanceof VehicleInstance;
		assert vehicle.setInstance("myBmw") instanceof VehicleInstance;
	}

	public void test007() {
		Root engine = new Root(VehicleType.class);
		VehicleType vehicle = engine.find(VehicleType.class);
		assert vehicle.getInstances().stream().allMatch(x -> x instanceof VehicleInstance);
	}

	public void test008() {
		Root engine = new Root(MyAudi.class);
		assert engine.find(MyAudi.class) instanceof VehicleInstance : engine.find(MyAudi.class).getClass();
		assert engine.find(MyAudi.class) instanceof MyAudi : engine.find(MyAudi.class).getClass();
	}

	public void test009() {
		catchAndCheckCause(() -> new Root(MyBmw.class), InstantiationException.class);
		catchAndCheckCause(() -> new Root(MyMercedes.class), InstantiationException.class);
	}

	public void test010() {
		Root engine = new Root(Vehicle.class, Car.class, myCar.class);
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

	public void test011() {
		Root engine = new Root(Vehicle.class, Power.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic power = engine.find(Power.class);
		assert power.isStructural();
		assert vehicle.getAttributes(engine).contains(power) : vehicle.getAttributes(engine);
	}

	public void test012() {
		Root engine = new Root(V123.class);
		Generic myVehicle = engine.find(MyVehicle.class);
		engine.find(V123.class);
		assert myVehicle.getValues(engine.find(Power.class)).size() == 1;
		assert myVehicle.getValues(engine.find(Power.class)).contains(new Integer(123)) : myVehicle.getValues(engine.find(Power.class));
	}

	public void test006_AttributeOnAttribute() {
		Root engine = new Root(ElectrikPower.class, Unit.class);
		Generic electrikPower = engine.find(ElectrikPower.class);
		Generic unit = engine.find(Unit.class);
		assert unit.isCompositeOf(electrikPower);
		assert unit.isStructural();
		assert electrikPower.getAttributes(engine).contains(unit);
	}

	public void test007_Relation() {
		Root engine = new Root(Vehicle.class, Human.class, HumanPossessVehicle.class);
		engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic possess = engine.find(HumanPossessVehicle.class);
		assert human.getRelations().contains(possess);
	}

	public void test008_SubRelation() {
		Root engine = new Root(Car.class, Human.class, HumanPossessCar.class);
		engine.find(Car.class);
		Generic human = engine.find(Human.class);
		Generic humanPossessCar = engine.find(HumanPossessCar.class);
		assert human.getRelations().contains(humanPossessCar);

	}

	public void test009_SymetricSuperRelation() {
		Root engine = new Root(Car.class, Human.class, Man.class, HumanPossessVehicle.class, ManPossessCar.class);
		engine.find(Car.class);
		Generic human = engine.find(Human.class);
		Generic man = engine.find(Man.class);
		Generic humanPossessVehicle = engine.find(HumanPossessVehicle.class);
		Generic manPossessCar = engine.find(ManPossessCar.class);
		assert human.getRelations().contains(humanPossessVehicle) : human.getAttributes();
		assert man.getRelations().contains(manPossessCar) : man.getAttributes();
		assert manPossessCar.inheritsFrom(humanPossessVehicle);
	}

	public void test010_TernaryRelation() {
		Root engine = new Root(Vehicle.class, Human.class, Time.class, HumanPossessVehicleTime.class);
		engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		engine.find(Time.class);
		Generic possess = engine.find(HumanPossessVehicleTime.class);
		assert human.getRelations().contains(possess);
	}

	public void test011_getDirectSubVertexsWithDiamondProblem() {
		Root engine = new Root(GraphicComposite.class, Window.class, Selectable.class, SelectableWindow.class);
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

		assert selectableWindow.inheritsFrom(selectable);
		assert selectableWindow.inheritsFrom(window);
		assert selectableWindow.inheritsFrom(graphicComposite);
	}

	public void test012_Value() {
		Root engine = new Root(SelectableWindow.class, Size.class, Selected.class, MySelectableWindow.class);
		Generic selectableWindow = engine.find(SelectableWindow.class);
		Generic size = engine.find(Size.class);
		Generic selected = engine.find(Selected.class);
		Generic mySelectableWindow = engine.find(MySelectableWindow.class);
		assert mySelectableWindow.isInstanceOf(selectableWindow) : mySelectableWindow.info() + selectableWindow.info();

		assert engine.find(Selectable.class).isAncestorOf(mySelectableWindow);
		Generic trueSelected = selected.addInstance(true, selected.getComponents().toArray(new Generic[1]));
		Generic twelveSize = size.addInstance(12, size.getComponents().toArray(new Generic[1]));

		assert selectableWindow.getInstances().size() == 1 : selectableWindow.getInstances();
		assert selectableWindow.getInstances().contains(mySelectableWindow);
		assert mySelectableWindow.getHolders(size).size() == 1 : mySelectableWindow.getHolders(size);
		assert mySelectableWindow.getHolders(size).contains(twelveSize) : mySelectableWindow.getHolders(size);
		assert mySelectableWindow.getHolders(selected).size() == 1;
		assert mySelectableWindow.getHolders(selected).contains(trueSelected);
	}

	public void test013_MultiInheritanceComplexStructural() {
		Root engine = new Root(Games.class, Children.class, Vehicle.class, Human.class, ChildrenGames.class, Transformer.class, TransformerChildrenGames.class);
		Generic games = engine.find(Games.class);
		Generic children = engine.find(Children.class);
		Generic vehicle = engine.find(Vehicle.class);
		Generic human = engine.find(Human.class);
		Generic childrenGames = engine.find(ChildrenGames.class);
		Generic transformer = engine.find(Transformer.class);
		Generic transformerChildrenGames = engine.find(TransformerChildrenGames.class);

		assert transformerChildrenGames.inheritsFrom(transformer);
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

		assert transformer.getSupers().size() == 2;
		assert transformer.getSupers().contains(vehicle);
		assert transformer.getSupers().contains(human);
		assert transformer.getInheritings().contains(transformerChildrenGames);
		assert transformer.getComposites().size() == 0;
	}

	public void test014_MultiInheritanceComplexValue() {
		Root engine = new Root(MyGames.class, MyChildren.class, MyVehicle.class, Myck.class, MyChildrenGames.class, MyTransformer.class, MyTransformerChildrenGames.class);
		Generic myVehicle = engine.find(MyVehicle.class);
		Generic myck = engine.find(Myck.class);
		Generic myTransformer = engine.find(MyTransformer.class);
		Generic myTransformerChildrenGames = engine.find(MyTransformerChildrenGames.class);

		assert !myTransformerChildrenGames.inheritsFrom(engine.find(MyGames.class));
		assert !myTransformerChildrenGames.inheritsFrom(engine.find(MyChildren.class));
		assert !myTransformerChildrenGames.inheritsFrom(myVehicle);
		assert !myTransformerChildrenGames.inheritsFrom(myck);
		assert !myTransformerChildrenGames.inheritsFrom(engine.find(MyChildrenGames.class));
		assert !myTransformerChildrenGames.inheritsFrom(myTransformer);
		assert myTransformerChildrenGames.getSupers().size() == 0;
		assert myTransformerChildrenGames.getInheritings().size() == 0;
		assert myTransformerChildrenGames.getComposites().size() == 0;

		assert !myTransformer.inheritsFrom(myVehicle);
		assert !myTransformer.inheritsFrom(myck);
		assert myTransformer.getSupers().size() == 0;
		assert myTransformer.getInheritings().size() == 0;
		assert myTransformer.getComposites().size() == 0;

	}

	public void testxxx() {
		Root engine = new Root(TransformerChildrenGames.class, MyTransformerChildrenGames.class);
		Generic transformerChildrenGames = engine.find(TransformerChildrenGames.class);
		Generic myTransformerChildrenGames = engine.find(MyTransformerChildrenGames.class);
		assert transformerChildrenGames.getInstances().contains(myTransformerChildrenGames);
		assert myTransformerChildrenGames.isInstanceOf(transformerChildrenGames) : myTransformerChildrenGames.info() + transformerChildrenGames.info();

		assert transformerChildrenGames.getSupers().size() == 2 : transformerChildrenGames.getSupers();
		assert transformerChildrenGames.getSupers().contains(engine.find(Transformer.class));
		assert transformerChildrenGames.getSupers().contains(engine.find(ChildrenGames.class));
	}

	public void testxxxx() {
		Root engine = new Root(ChildrenGames.class, MyChildrenGames.class, MyGames.class, MyChildren.class);
		Generic myChildrenGames = engine.find(MyChildrenGames.class);
		Generic childrenGames = engine.find(ChildrenGames.class);
		assert !myChildrenGames.inheritsFrom(engine.find(MyGames.class));
		assert !myChildrenGames.inheritsFrom(engine.find(MyChildren.class));
		assert myChildrenGames.getSupers().size() == 0;
		assert myChildrenGames.getInheritings().size() == 0;
		assert myChildrenGames.getComposites().size() == 0;

		assert childrenGames.getSupers().size() == 2;
		assert childrenGames.getSupers().contains(engine.find(Games.class));
		assert childrenGames.getSupers().contains(engine.find(Children.class));

		assert childrenGames.getInstances().contains(myChildrenGames);
		assert myChildrenGames.isInstanceOf(childrenGames);
	}

	public void test015_propertyConstraint() {
		Root engine = new Root(Options.class);
		Generic options = engine.find(Options.class);
		assert options.isPropertyConstraintEnabled();
	}

	public void test017_singularConstraint() {
		Root engine = new Root(Options.class);
		Generic options = engine.find(Options.class);
		assert options.isSingularConstraintEnabled(0);
	}

	public void test018_uniqueValueConstraint() {
		Root engine = new Root(Options.class);
		Generic options = engine.find(Options.class);
		assert options.isUniqueValueEnabled();
	}

	public void test019_uniqueClassConstraint() {
		Root engine = new Root(Options.class);
		Generic options = engine.find(Options.class);
		assert options.getInstanceValueClassConstraint().equals(Integer.class);
	}

	public void test020_dependencies() {
		Root engine = new Root(MicroCar.class);
		Generic options = engine.find(Options.class);
		Generic music = engine.find(Music.class);
		assert options instanceof Options;
		assert music instanceof Music;
	}

	public void testdsd() {
		Root engine = new Root(Power.class);
		Generic power = engine.find(Power.class);
		assert power.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
	}

	/**
	 * Fin des tests ----------------------------------
	 */

	@SystemGeneric
	public static interface Vehicle extends Generic {

	}

	@SystemGeneric
	public static interface OtherVehicle {

	}

	public static interface VehicleInstance extends Generic {
	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static interface MyAudi extends VehicleInstance {
	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static interface MyBmw extends Generic {

	}

	@SystemGeneric
	@Meta(VehicleType.class)
	public static interface MyMercedes {
	}

	@SystemGeneric
	@InstanceClass(VehicleInstance.class)
	public static interface VehicleType extends Generic {

	}

	@SystemGeneric
	@InstanceClass(VehicleInstance.class)
	public static interface OtherVehicleType {

	}

	@SystemGeneric
	public static interface Games extends Generic {
	}

	@SystemGeneric
	@Meta(Games.class)
	public static interface MyGames extends Generic {
	}

	@SystemGeneric
	public static interface Children extends Generic {
	}

	@SystemGeneric
	@Meta(Children.class)
	public static interface MyChildren extends Generic {
	}

	@SystemGeneric
	@Supers({ Games.class, Children.class })
	public static interface ChildrenGames extends Generic {
	}

	@SystemGeneric
	@Meta(ChildrenGames.class)
	public static interface MyChildrenGames extends Generic {
	}

	@SystemGeneric
	@Supers({ Human.class, Vehicle.class })
	public static interface Transformer extends Generic {
	}

	@SystemGeneric
	@Meta(Transformer.class)
	public static interface MyTransformer extends Generic {
	}

	@SystemGeneric
	@Supers({ Transformer.class, ChildrenGames.class })
	public static interface TransformerChildrenGames extends Generic {
	}

	@SystemGeneric
	@Meta(TransformerChildrenGames.class)
	public static interface MyTransformerChildrenGames extends Generic {
	}

	@SystemGeneric
	@Meta(Vehicle.class)
	public static interface MyVehicle extends Generic {
	}

	@SystemGeneric
	@Components(Vehicle.class)
	@RequiredConstraint
	public static interface Power extends Generic {

	}

	@SystemGeneric
	@Components(Vehicle.class)
	@PropertyConstraint
	@SingularConstraint(ApiStatics.BASE_POSITION)
	@UniqueValueConstraint
	@InstanceValueClassConstraint(Integer.class)
	@Dependencies(Music.class)
	public static interface Options extends Generic {

	}

	public static interface Music extends Generic {

	}

	@Dependencies(Options.class)
	public static interface MicroCar extends Generic {

	}

	@SystemGeneric
	@Meta(Power.class)
	@Components(MyVehicle.class)
	@IntValue(123)
	public static interface V123 extends Generic {

	}

	@SystemGeneric
	@Supers(Vehicle.class)
	public static interface Car extends Generic {

	}

	@SystemGeneric
	@Meta(Car.class)
	public static interface myCar extends Generic {
	}

	@SystemGeneric
	@Components(Car.class)
	@Supers(Power.class)
	public static interface ElectrikPower extends Generic {

	}

	@SystemGeneric
	@Components(ElectrikPower.class)
	public static interface Unit extends Generic {

	}

	@SystemGeneric
	public static interface Human extends Generic {
	}

	@SystemGeneric
	public static interface Man extends Human {
	}

	@SystemGeneric
	@Meta(Human.class)
	public static interface Myck extends Generic {
	}

	@SystemGeneric
	public static interface Time extends Generic {
	}

	@SystemGeneric
	@Components({ Human.class, Vehicle.class })
	public static interface HumanPossessVehicle extends Generic {
	}

	@SystemGeneric
	@Components({ Human.class, Car.class })
	@Supers(HumanPossessVehicle.class)
	public static interface HumanPossessCar extends HumanPossessVehicle {
	}

	@SystemGeneric
	@Components({ Man.class, Car.class })
	@Supers(HumanPossessVehicle.class)
	public static interface ManPossessCar extends HumanPossessVehicle {
	}

	@SystemGeneric
	@Components({ Human.class, Vehicle.class, Time.class })
	public static interface HumanPossessVehicleTime extends Generic {
	}

	@SystemGeneric
	public static interface GraphicComposite extends Generic {

	}

	@SystemGeneric
	@Components(GraphicComposite.class)
	public static interface Size extends Generic {

	}

	@SystemGeneric
	@Supers(GraphicComposite.class)
	public static interface Window extends GraphicComposite {

	}

	@SystemGeneric
	@Supers(GraphicComposite.class)
	public static interface Selectable extends Generic {

	}

	@SystemGeneric
	@Components(Selectable.class)
	public static interface Selected extends Generic {

	}

	@SystemGeneric
	@Supers({ Selectable.class, Window.class })
	public static interface SelectableWindow extends Generic {

	}

	@SystemGeneric
	@Meta(SelectableWindow.class)
	public static interface MySelectableWindow extends Generic {

	}

}
