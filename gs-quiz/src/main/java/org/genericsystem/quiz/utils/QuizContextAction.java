package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.app.pages.HomePage;
import org.genericsystem.quiz.app.pages.QuizPage;
import org.genericsystem.quiz.app.pages.ResultPage;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.FinishBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.NextBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.PreviousBtn;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.security.model.User;

public class QuizContextAction {

	public static class QuizNextAction implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Class<?> from = tag.getParent().getClass();
			Class<?> to = null;
			if (HomePage.class.equals(from))
				to = QuizPage.class;
			if (QuizPage.class.equals(from))
				to = ResultPage.class;
			if (ResultPage.class.equals(from))
				to = HomePage.class;
			tag.getProperty("selectedClass", context).setValue(to);
		}
	}

	public static class NEXT_TAG implements ContextAction {
		@Override
		public void accept(Context context, Tag tagNext) {
			if (QuizStepper.class.isAssignableFrom(tagNext.getClass())) {

				Tag tagParent = tagNext.getParent();
				Tag tagPrevious = tagParent.find(PreviousBtn.class);
				Tag tagFinish = tagParent.find(FinishBtn.class);

				((QuizStepper) tagNext).next(context, tagNext, tagPrevious, tagFinish);

			} else
				log.warn("The NEXT action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class PREVIOUS_TAG implements ContextAction {
		@Override
		public void accept(Context context, Tag tagPrevious) {
			if (QuizStepper.class.isAssignableFrom(tagPrevious.getClass())) {

				Tag tagParent = tagPrevious.getParent();
				Tag tagNext = tagParent.find(NextBtn.class);
				Tag tagFinish = tagParent.find(FinishBtn.class);

				((QuizStepper) tagPrevious).prev(context, tagNext, tagPrevious, tagFinish);

			} else
				log.warn("The PREVIOUS action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class SAVE_QUIZ_RESULT implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {

			Generic quiz = context.getGeneric();
			Generic sUser = context.find(User.class).getInstance("Anti-Seche");
			Generic loggedUser = tag.getLoggedUserProperty(context).getValue();

			context.flush();

			ScoreUtils.setResult(context, quiz, sUser, loggedUser);
			ScoreUtils.getResult(context, quiz, loggedUser);

			tag.getProperty("Quiz Done", context).setValue(true);
		}
	}

	public static class SELECT_CONTEXT implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).getValue();
		}

	}
}
