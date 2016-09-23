package org.genericsystem.reactor.gs3;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.AlignItems;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.Color;
import org.genericsystem.reactor.annotations.Style.Flex;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.JustifyContent;
import org.genericsystem.reactor.annotations.Style.MarginBottom;
import org.genericsystem.reactor.annotations.Style.MarginRight;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSInputTextWithConversion;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.BooleanHolderEditor.CheckboxContainerDiv.BooleanHolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.HolderEditor.HolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.DirectRelationComponentEditor;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.RemovalLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.MultiCheckbox.CheckboxLabel.Checkbox;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceNameEditor;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.InstanceType;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.InstanceType.TypeNameDisplayer;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.TypeAttribute.AttributeName.AttributeNameDisplayer;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.TypeAttribute.RelationName.ComponentName.ComponentNameDisplayer;
import org.genericsystem.reactor.gs3.GSEditor.EditorTitle.EditorTitleContent;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_2;
import org.genericsystem.reactor.model.ObservableValueSelector.RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@ReactorDependencies({ EditorTitleContent.class, InstanceType.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceNameEditor.class, Checkbox.class, ReversedRelationDisplayer.class,
		DirectRelationComponentEditor.class, BooleanHolderEditorInput.class, HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class, BooleanHolderAdditionLink.class, HolderAdderInput.class, HolderAdditionLink.class,
		ComponentAdderSelect.class })
@Flex("1")
@FlexDirection("row")
public class GSEditor extends CompositeTagImpl {

	public GSEditor() {
		super();
	}

	public GSEditor(Tag parent) {
		super(parent);
	}

	// No automatic enclosing class parent for this GSEditor extension
	@FlexDirection("column")
	public static class HorizontalGSEditor extends GSEditor implements SelectionDefaults {

		public HorizontalGSEditor(Tag parent) {
			super(parent);
		}

		// @Override
		// public void style() {
		// ColumnFlexStyle.super.style();
		// }
	}

	// Main title.
	@BackgroundColor("#EA4500")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("White")
	@JustifyContent("center")
	@AlignItems("center")
	public static class EditorTitle extends GSDiv {

		public static class EditorTitleContent extends HtmlH2 {

			@Override
			public void init() {
				setStringExtractor(StringExtractor.TYPE_INSTANCE_EXTRACTOR);
				bindText();
			}
		}
	}

	// Content.
	@Parent(GSEditor.class)
	// keepDirection();
	@Flex("1")
	@Style(propertyName = "height", propertyValue = "100%")
	public static class EditorContent extends GSDiv {
		// Line/column with the names of the attributes and components of relations.
		public static class LinkTitles extends GSDiv implements FlexStyle.LinkTitlesStyle {
			public static class InstanceType extends GSDiv implements FlexStyle.TitleLineCellStyle {

				@Override
				public void init() {
					select(gs -> gs[1]);
				}

				public static class TypeNameDisplayer extends GSLabelDisplayer {

				}
			}

			@ForEach(ATTRIBUTES_OF_INSTANCES.class)
			public static class TypeAttribute extends GSDiv implements FlexStyle.RowFlexStyle {

				@Select(STRICT_ATTRIBUTE_SELECTOR.class)
				public static class AttributeName extends GSDiv implements FlexStyle.TitleLineCellStyle {

					public static class AttributeNameDisplayer extends GSLabelDisplayer {

					}
				}

				@Select(RELATION_SELECTOR.class)
				public static class RelationName extends GSDiv implements FlexStyle.RowFlexStyle {

					@ForEach(OTHER_COMPONENTS_2.class)
					public static class ComponentName extends GSDiv implements FlexStyle.TitleLineCellStyle {

						public static class ComponentNameDisplayer extends GSLabelDisplayer {
						}
					}
				}
			}
		}

		// Edition itself.
		public static class InstanceEdition extends GSDiv implements FlexStyle.ReversedFlexStyle {

			// Edition of the name of the instance.
			public static class InstanceNameEditorDiv extends GSDiv implements FlexStyle.SubCellEditorStyle {

				public static class InstanceNameEditor extends GSInputTextEditorWithConversion implements FullSizeStyle {
				}
			}

			// Edition of the holders/links.
			public static class InstanceAttributeEditor extends GSDiv implements FlexStyle.AttributeEditorStyle {

				@Override
				public void init() {
					forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				}

				// Multiple checkboxes : for binary relations without the singular constraint.
				public static class MultiCheckbox extends GSDiv implements FlexStyle.MultiCheckboxStyle {

					@Override
					public void init() {
						select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
					}

					public static class CheckboxLabel extends org.genericsystem.reactor.gstag.HtmlLabel implements FlexStyle.CheckboxLabelStyle {

						@Override
						public void init() {
							bindText();
							forEach(gs -> ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])).stream().toArray(Generic[]::new)));
						}

						public static class Checkbox extends GSCheckBoxWithValue implements CheckboxStyle {

							@Override
							public void init() {
								initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
								storeProperty("exists", context -> {
									ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()).getValue() != null ? true : false,
											context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()));
									exists.addListener((o, v, nva) -> {
										if (!context.isDestroyed())
											getConvertedValueProperty(context).setValue(nva);
									});
									return exists;
								});
								addConvertedValueChangeListener((context, nva) -> {
									if (Boolean.TRUE.equals(nva))
										context.getGenerics()[2].setHolder(context.getGenerics()[1], null, context.getGeneric());
									if (Boolean.FALSE.equals(nva)) {
										Generic link = context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric());
										if (link != null)
											link.remove();
									}
								});
							}
						}
					}
				}

				// Edition of other attributes.
				public static class AttributeEditionColumn extends GenericColumn implements FlexStyle.AttributeEditionColumnStyle {

					@Override
					public void init() {
						select(gs -> gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
					}

					public static class SubcellEditor extends GSDiv implements FlexStyle.SubcellEditorContainerStyle {

						@Override
						public void init() {
							forEach(ObservableListExtractor.HOLDERS);
						}

						// Edition of non-boolean holders.
						public static class HolderEditor extends GSDiv implements FlexStyle.SubCellEditorStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							public static class HolderEditorInput extends GSInputTextEditorWithConversion implements FullSizeStyle {
							}

						}

						// Edition of boolean holders.
						public static class BooleanHolderEditor extends GSDiv implements FlexStyle.SubCellEditorStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							public static class CheckboxContainerDiv extends GSDiv implements FlexStyle.CenteredFlex {
								@Parent(CheckboxContainerDiv.class)
								public static class BooleanHolderEditorInput extends GSCheckBoxEditor {
								}
							}

						}

						// Edition of links.
						public static class LinkEditor extends GSDiv implements org.genericsystem.reactor.modelproperties.ComponentsDefaults, FlexStyle.RowFlexStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
								createComponentsListProperty();
							}

							public static class ComponentEditor extends GSDiv implements ComponentEditorStyle {

								@Override
								public void init() {
									forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
								}

								// TODO: Finish decomposition of InstanceCompositeSelect.
								public static class DirectRelationComponentEditor extends InstanceCompositeSelect implements FullSizeStyle {

									@Override
									public void init() {
										select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
										addPostfixBinding(model -> {
											Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
											if (selectedComponents != null)
												selectedComponents.getValue().add(getSelectionProperty(model));
										});
									}
								}

								public static class ReversedRelationDisplayer extends GSLabelDisplayer {

									@Override
									public void init() {
										select(gs -> !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) && !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null);
									}
								}

							}

						}

						// Hyperlink to remove a holder/link. Displayed only if there is no required constraint on the given attribute/relation,
						// or there is a required constraint but there are at least two holders/links for the given attribute/relation.
						public static class RemovalLink extends HtmlHyperLink implements ActionLinkStyle {

							@Override
							public void init() {
								setText("×");
								bindAction(Context::remove);
								select__(context -> {
									ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getParent().getGenerics());
									return BindingsTools.transmitSuccessiveInvalidations(
											Bindings.createObjectBinding(() -> (!context.getParent().getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? context : null, holders));
								});
							}
						}

					}

					// To add a new holder/link if it’s possible.
					public static class SubcellAdder extends GSDiv implements FlexStyle.SubcellEditorContainerStyle {

						@Override
						public void init() {
							select__(model -> {
								ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
								return Bindings
										.createObjectBinding(
												() -> holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
														|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? model : null,
												ObservableListExtractor.HOLDERS.apply(model.getGenerics()));
							});
						}

						// Addition of non-boolean holders.
						public static class HolderAdder extends GSDiv implements FlexStyle.SubCellEditorStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							public static class HolderAdderInput extends GSInputTextWithConversion implements FullSizeStyle {

								@Override
								public void init() {
									addConvertedValueChangeListener((model, nva) -> {
										if (nva != null)
											model.getGenerics()[1].addHolder(model.getGeneric(), nva);
									});
								}
							}

							public static class HolderAdditionLink extends AdditionLink {

								@Override
								public void postfix() {
									bindAction(context -> addHolder(context, (ConvertedValueDefaults) find(HolderAdderInput.class)));
								}
							}
						}

						// Addition of boolean holders.
						public static class BooleanHolderAdder extends GSDiv implements FlexStyle.SubCellEditorStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							public static class CheckboxContainerAddDiv extends GSDiv implements FlexStyle.CenteredFlex {
								public static class BooleanHolderAdderInput extends GSCheckBoxWithValue {

									@Override
									public void init() {
										addConvertedValueChangeListener((model, nva) -> {
											if (nva != null)
												model.getGenerics()[1].addHolder(model.getGeneric(), nva);
										});
									}
								}

								public static class BooleanHolderAdditionLink extends AdditionLink {

									@Override
									public void postfix() {
										bindAction(context -> addHolder(context, (ConvertedValueDefaults) find(BooleanHolderAdderInput.class)));
									}
								}
							}
						}

						// Addition of links.
						public static class LinkAdder extends GSDiv implements org.genericsystem.reactor.modelproperties.ComponentsDefaults, FlexStyle.RowFlexStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
								createComponentsListProperty();
								addPostfixBinding(model -> {
									Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
									ChangeListener<Context> listener = (o, v, nva) -> {
										List<Generic> selectedGenerics = selectedComponents.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
										if (selectedGenerics.size() + 1 == model.getGeneric().getComponents().size()) {
											selectedComponents.getValue().stream().forEach(sel -> sel.setValue(null));
											try {
												model.getGenerics()[1].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
											} catch (RollbackException e) {
												e.printStackTrace();
											}
										}
									};
									selectedComponents.getValue().forEach(component -> component.addListener(listener));
								});
							}

							public static class ComponentAdder extends GSDiv implements ComponentEditorStyle {

								@Override
								public void init() {
									forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
								}

								// TODO: Finish decomposition of CompositeSelectWithEmptyEntry.
								@Parent(ComponentAdder.class)
								public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry implements FullSizeStyle {

									@Override
									public void init() {
										select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
										addPostfixBinding(model -> {
											Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
											if (selectedComponents != null)
												selectedComponents.getValue().add(getSelectionProperty(model));
										});
									}
								}
							}
						}
					}
				}

				// Hyperlink to create the holder. Displayed only for holders, not for links.
				public static class AdditionLink extends HtmlHyperLink implements ActionLinkStyle {

					@Override
					public void init() {
						setText("+");
					}

					protected void addHolder(Context context, ConvertedValueDefaults tag) {
						Property<Serializable> observable = tag.getConvertedValueProperty(context);
						if (observable.getValue() != null) {
							Serializable newValue = observable.getValue();
							observable.setValue(null);
							context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
						}
					}
				}
			}
		}
	}
}
