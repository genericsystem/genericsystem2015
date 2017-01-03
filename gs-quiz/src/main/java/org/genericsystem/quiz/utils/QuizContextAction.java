package org.genericsystem.quiz.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.FinishBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.NextBtn;
import org.genericsystem.quiz.components.QuestionDiv.QuestionDiv_.FooterDiv.PreviousBtn;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.security.model.User;

public class QuizContextAction {

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
			System.out.println("Scores -> ");
			ScoreUtils.getScores(context, quiz).forEach(System.out::println);

			System.out.println("Note de " + loggedUser + " -> " + ScoreUtils.calculateSimpleGrade(context, quiz, loggedUser) + " / 20");

		}
	}
}
