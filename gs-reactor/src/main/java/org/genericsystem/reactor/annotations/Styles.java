package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface Styles {
	Style[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(Styles.class)
	public @interface Style {
		String propertyName();

		String propertyValue();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexDirectionStyle {
		FlexDirection value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface KeepFlexDirection {
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ReverseFlexDirection {
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Flex {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexWrap {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface BackgroundColor {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface GenericBackgroundColor {
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface AlignItems {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface JustifyContent {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Overflow {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Color {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface MarginRight {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface MarginBottom {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Height {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Width {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildStyles.class)
	public @interface ChildStyle {
		Class<? extends GSTagImpl> decorate();

		String name();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildStyles {
		ChildStyle[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildFlexDirections.class)
	public @interface ChildFlexDirection {
		Class<? extends GSTagImpl> decorate();

		FlexDirection value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildFlexDirections {
		ChildFlexDirection[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildKeepFlexDirection {
		Class<? extends GSTagImpl>[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildReverseFlexDirection {
		Class<? extends GSTagImpl>[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildFlexs.class)
	public @interface ChildFlex {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildFlexs {
		ChildFlex[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildFlexWraps.class)
	public @interface ChildFlexWrap {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildFlexWraps {
		ChildFlexWrap[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildBackgroundColors.class)
	public @interface ChildBackgroundColor {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildBackgroundColors {
		ChildBackgroundColor[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildGenericBackgroundColors.class)
	public @interface ChildGenericBackgroundColor {
		Class<? extends GSTagImpl> decorate();

	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildGenericBackgroundColors {
		ChildGenericBackgroundColor[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildAlignItemss.class)
	public @interface ChildAlignItems {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildAlignItemss {
		ChildAlignItems[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildJustifyContents.class)
	public @interface ChildJustifyContent {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildJustifyContents {
		ChildJustifyContent[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildOverflows.class)
	public @interface ChildOverflow {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildOverflows {
		ChildOverflow[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildColors.class)
	public @interface ChildColor {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildColors {
		ChildColor[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildMarginRights.class)
	public @interface ChildMarginRight {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildMarginRights {
		ChildMarginRight[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildMarginBottoms.class)
	public @interface ChildMarginBottom {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildMarginBottoms {
		ChildMarginBottom[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildHeights.class)
	public @interface ChildHeight {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildHeights {
		ChildHeight[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(ChildWidths.class)
	public @interface ChildWidth {
		Class<? extends GSTagImpl> decorate();

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildWidths {
		ChildWidth[] value();
	}
}
