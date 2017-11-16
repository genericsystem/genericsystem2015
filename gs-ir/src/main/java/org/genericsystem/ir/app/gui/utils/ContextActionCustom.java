package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;

/**
 * This class contains all the {@link ContextAction} needed across the app.
 */
public class ContextActionCustom {

	/*
	 * === PAGE NAVIGATION ===
	 */

	// public static class CALL_STATISTICS_PAGE implements ContextAction {
	// @Override
	// public void accept(Context context, Tag tag) {
	// System.out.println("Redirecting to statistics page");
	// tag.setInheritedContextPropertyValue(PageSwitcher.PAGE, context, PageSwitcher.FILTERS_STATISTICS);
	// }
	// }

	public static class CALL_HOME_PAGE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Redirecting to home page");
			tag.setInheritedContextPropertyValue(PageSwitcher.PAGE, context, PageSwitcher.HOME_PAGE);
		}
	}

	public static class CALL_CLASSIFIER_PAGE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Redirecting to classifier page");
			tag.setInheritedContextPropertyValue(PageSwitcher.PAGE, context, PageSwitcher.CLASSIFIER_PAGE);
		}
	}

	public static class TEST implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("called from " + tag.getTag());
		}
	}

	/*
	 * === GENERIC SYSTEM ===
	 */

	public static class REMOVE_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.remove();
			// context.flush();
		}
	}

	public static class SAVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Saving...");
			Root root = context.getGeneric().getRoot();
			System.out.println("Current thread (save): " + Thread.currentThread().getName());
			long start = System.nanoTime();
			root.getCurrentCache().flush();
			long stop = System.nanoTime();
			System.out.println("Saved in " + (stop - start) / 1_000_000 + "ms");
		}
	}

	public static class MODAL_DISPLAY_FLEX_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Tag ancestor = tag.getParent().getParent(); // ZoneTextDiv
			ancestor.find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
		}
	}

	// public static class UPDATE_DOCCLASS implements ContextAction {
	// @Override
	// public void accept(Context context, Tag tag) {
	// DocInstance currentDoc = (DocInstance) context.getGeneric();
	// DocClassInstance currentDocClass = (DocClassInstance) context.getGenerics()[1];
	// DocClassInstance newdocClass = (DocClassInstance) tag.getContextProperty(HomePageTable.DOCCLASS_CONTEXT_PROPERTY, context).getValue();
	//
	// if (newdocClass != null && newdocClass != currentDocClass) {
	// System.out.println("updating components...");
	// currentDoc = (DocInstance) currentDoc.updateComponent(newdocClass, ApiStatics.BASE_POSITION);
	// System.out.println("done!");
	// }
	// }
	// }

	/*
	 * === BEST OCR TEXT ===
	 */

	public static class REFRESH_BEST_TEXT implements ContextAction {
		@Override
		public void accept(Context gsContext, Tag tag) {
			System.out.println("Refreshing best text...");
			System.err.println("NOT YET IMPLEMENTED!");
			// TODO: re-launch the OCR process on this specific file in the background
			// Root root = gsContext.getGeneric().getRoot();
			// DocInstance docInstance = (DocInstance) gsContext.getGeneric();
			//
			// WorkerVerticle worker = new WorkerVerticle(root) {
			// @Override
			// public void start() {
			// tag.addAttribute(gsContext, "disabled", "true");
			// gsContext.getCache().safeExecute(() -> {
			// try {
			// compute();
			// gsContext.getCache().flush();
			// } catch (Exception e) {
			// System.err.println("An error has occured, rollling back...");
			// e.printStackTrace();
			// gsContext.getCache().clear();
			// }
			// });
			// tag.addAttribute(gsContext, "disabled", "");
			// System.out.println("Done!");
			// }
			//
			// private void compute() {
			// ComputeBestTextPerZone.computeOneFile(root, docInstance);
			// docInstance.setRefreshTimestamp(ModelTools.getCurrentDate());
			// }
			// };
			// worker.deployAsWorkerVerticle("Failed to execute the task");
		}
	}

	/*
	 * === STATISTICS ===
	 */

	// public static class COMPUTE_STATS implements ContextAction {
	// @Override
	// public void accept(Context context, Tag tag) {
	// System.out.println("Computing scores...");
	// computeStatistics(context, tag, ComputeTrainedScores.BE_GENTLE);
	// }
	// }
	//
	// public static class COMPUTE_STATS_STRICT implements ContextAction {
	// @Override
	// public void accept(Context context, Tag tag) {
	// System.out.println("Computing scores (using strict mode)...");
	// computeStatistics(context, tag, ComputeTrainedScores.BE_STRICT);
	// }
	// }
	//
	// public static void computeStatistics(Context gsContext, Tag tag, boolean useStrict) {
	// DocClassInstance docClassInstance = (DocClassInstance) gsContext.getGeneric();
	// Root root = docClassInstance.getRoot();
	//
	// WorkerVerticle worker = new WorkerVerticle() {
	// @Override
	// public void start() {
	// tag.addAttribute(gsContext, "disabled", "true");
	// gsContext.getCache().safeExecute(() -> {
	// try {
	// ComputeTrainedScores.compute(root, docClassInstance.getValue().toString(), useStrict);
	// gsContext.getCache().flush();
	// } catch (Exception e) {
	// System.err.println("An error has occured, rollling back...");
	// e.printStackTrace();
	// gsContext.getCache().clear();
	// }
	// });
	// tag.addAttribute(gsContext, "disabled", null);
	// System.out.println("Done computing scores!");
	// }
	// };
	// worker.deployAsWorkerVerticle("Failed to execute the task");
	// }

	/*
	 * === LOGIN ===
	 */

	public static class CREATE_USER_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.mount();

			HtmlInputText userNameInput = tag.getParent().getParent().find(HtmlInputText.class);
			HtmlInputText passwordInput = tag.getParent().getParent().find(HtmlInputText.class, 1);
			HtmlInputText confirmPassword = tag.getParent().getParent().find(HtmlInputText.class, 2);
			HtmlSpan invalidUsername = tag.getParent().getParent().find(HtmlSpan.class);
			HtmlSpan invalidConfirmPassword = tag.getParent().getParent().find(HtmlSpan.class, 1);

			String userName = userNameInput.getDomNodeAttributes(context).get("value");
			String psw1 = passwordInput.getDomNodeAttributes(context).get("value");
			String psw2 = confirmPassword.getDomNodeAttributes(context).get("value");
			Generic user;

			if (userName != null) {
				try {
					user = context.find(User.class).addInstance(userName);
				} catch (RollbackException e) {
					invalidUsername.setText(context, "This username already exists");
					invalidUsername.addStyle(context, "display", "inline");
					invalidConfirmPassword.addStyle(context, "display", "none");
					return;
				}
				if (psw1 != null) {
					if (!psw1.isEmpty()) {
						if (psw1.equals(psw2)) {
							invalidUsername.addStyle(context, "display", "none");
							invalidConfirmPassword.addStyle(context, "display", "none");
							byte[] salt = EncryptionUtils.generateSalt();
							byte[] hash = EncryptionUtils.getEncryptedPassword(psw1, salt);
							Generic hashGeneric = user.setHolder(context.find(Password.class), hash);
							hashGeneric.setHolder(context.find(Salt.class), salt);
							tag.getDisplayProperty(context).setValue("none");
							userNameInput.getDomNodeAttributes(context).put("value", "");
							passwordInput.getDomNodeAttributes(context).put("value", "");
							confirmPassword.getDomNodeAttributes(context).put("value", "");
							context.flush();
							context.unmount();
							context.flush();
						} else { // Non-matching passwords
							invalidConfirmPassword.setText(context, "The passwords do not match");
							invalidUsername.addStyle(context, "display", "none");
							invalidConfirmPassword.addStyle(context, "display", "inline");
							context.unmount();
						}
					} else { // Password not null, but empty
						invalidConfirmPassword.setText(context, "Password can not be empty");
						invalidUsername.addStyle(context, "display", "none");
						invalidConfirmPassword.addStyle(context, "display", "inline");
					}
				} else { // Empty password
					invalidConfirmPassword.setText(context, "Password can not be empty");
					invalidUsername.addStyle(context, "display", "none");
					invalidConfirmPassword.addStyle(context, "display", "inline");
				}
			} else { // Empty username
				invalidUsername.setText(context, "Username can not be empty");
				invalidUsername.addStyle(context, "display", "inline");
				invalidConfirmPassword.addStyle(context, "display", "none");
			}
		}
	}
}
