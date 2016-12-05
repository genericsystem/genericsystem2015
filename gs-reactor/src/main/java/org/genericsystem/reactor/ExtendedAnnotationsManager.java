package org.genericsystem.reactor;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.gscomponents.TagImpl;

public class ExtendedAnnotationsManager extends AnnotationsManager {

	public ExtendedAnnotationsManager(Class<? extends RootTag> clazz) {
		super(clazz);

	}

	@Override
	public void initManager() {
		registerAnnotation(DirectSelect.class);
		registerAnnotation(Select.class);
		registerAnnotation(SelectContext.class);
		registerAnnotation(ForEach.class);
		registerAnnotation(Stepper.class);
		registerAnnotation(BindSelection.class);
		registerAnnotation(SetStringExtractor.class);
		registerAnnotation(StyleClass.class);
		registerAnnotation(FlexDirectionStyle.class);
		registerAnnotation(KeepFlexDirection.class);
		registerAnnotation(ReverseFlexDirection.class);
		registerAnnotation(SetText.class);
		registerAnnotation(BindText.class);
		registerAnnotation(BindAction.class);
		// registerAnnotation(Style.class);
		registerAnnotation(GenericValueBackgroundColor.class);
		registerAnnotation(Attribute.class);
		registerAnnotation(Switch.class);
	}

	@Override
	public void processChildrenAnnotations(Tag tag) {
		((TagImpl) tag).setTagNode(tag.getRootTag().buildTagNode(tag));
	}
}
