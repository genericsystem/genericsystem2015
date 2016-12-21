package org.genericsystem.reactor;

import java.util.Map.Entry;

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
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GenericTagNode;
import org.genericsystem.reactor.gscomponents.FlexDirection;
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
		// registerAnnotation(StyleClass.class);
		// registerAnnotation(FlexDirectionStyle.class);
		// registerAnnotation(KeepFlexDirection.class);
		// registerAnnotation(ReverseFlexDirection.class);
		// registerAnnotation(SetText.class);
		// registerAnnotation(BindText.class);
		// registerAnnotation(BindAction.class);
		// registerAnnotation(Style.class);
		// registerAnnotation(GenericValueBackgroundColor.class);
		// registerAnnotation(Attribute.class);
		registerAnnotation(Switch.class);
	}

	@Override
	public void processChildrenAnnotations(Tag tag) {
		((TagImpl) tag).setTagNode(tag.getRootTag().buildTagNode(tag));
	}

	@Override
	public void processAnnotations(Tag tag) {
		super.processAnnotations(tag);
		processStoredAnnotations(tag);
	}

	public void processStoredAnnotations(Tag tag) {
		GenericTagNode tagNode = (GenericTagNode) tag.getTagNode();

		Entry<GTagAnnotation, GTagAnnotationContent> styleClassAnnotation = tagNode.getTagAnnotation(StyleClass.class);
		if (styleClassAnnotation != null)
			styleClassAnnotation.getValue().getContentJSonArray().forEach(styleClass -> tag.addStyleClass((String) styleClass));

		Entry<GTagAnnotation, GTagAnnotationContent> flexDirection = tagNode.getTagAnnotation(FlexDirectionStyle.class);
		if (flexDirection != null)
			tag.getRootTag().processFlexDirectionStyle(tag, FlexDirection.valueOf(flexDirection.getValue().getContentValue()));

		Entry<GTagAnnotation, GTagAnnotationContent> keepFlexDirection = tagNode.getTagAnnotation(KeepFlexDirection.class);
		if (keepFlexDirection != null)
			tag.getRootTag().processKeepFlexDirection(tag);

		Entry<GTagAnnotation, GTagAnnotationContent> reverseFlexDirection = tagNode.getTagAnnotation(ReverseFlexDirection.class);
		if (reverseFlexDirection != null)
			tag.getRootTag().processReverseFlexDirection(tag);

		Entry<GTagAnnotation, GTagAnnotationContent> setText = tagNode.getTagAnnotation(SetText.class);
		if (setText != null)
			tag.getRootTag().processSetText(tag, setText.getKey().getValue().getPath(), setText.getValue().getContentJSonArray().stream().toArray(String[]::new));

		Entry<GTagAnnotation, GTagAnnotationContent> bindText = tagNode.getTagAnnotation(BindText.class);
		if (bindText != null)
			tag.getRootTag().processBindText(tag, (Class<? extends TextBinding>) bindText.getValue().getClassContent());

		Entry<GTagAnnotation, GTagAnnotationContent> bindAction = tagNode.getTagAnnotation(BindAction.class);
		if (bindAction != null)
			tag.getRootTag().processBindAction(tag, (Class<? extends ContextAction>) bindAction.getValue().getClassContent());

		for (Entry<GTagAnnotation, GTagAnnotationContent> tagAnnotation : tagNode.getTagAnnotations(Style.class).entrySet())
			tag.addStyle(tagAnnotation.getKey().getValue().getName(), tagAnnotation.getValue().getContentValue());

		Entry<GTagAnnotation, GTagAnnotationContent> gvbColor = tagNode.getTagAnnotation(GenericValueBackgroundColor.class);
		if (gvbColor != null)
			tag.getRootTag().processGenericValueBackgroundColor(tag, gvbColor.getValue().getContentValue());

		for (Entry<GTagAnnotation, GTagAnnotationContent> tagAnnotation : tagNode.getTagAnnotations(Attribute.class).entrySet())
			tag.addAttribute(tagAnnotation.getKey().getValue().getName(), tagAnnotation.getValue().getContentValue());
	}
}
