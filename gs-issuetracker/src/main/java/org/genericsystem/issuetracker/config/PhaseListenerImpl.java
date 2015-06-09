package org.genericsystem.issuetracker.config;

import java.util.logging.Logger;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

public class PhaseListenerImpl implements PhaseListener {

	private static final long serialVersionUID = 3090808698206316179L;
	private static final Logger log = Logger.getAnonymousLogger();

	@Override
	public void beforePhase(PhaseEvent e) {
		log.info("\n\n\n---------------------------------" + e.getPhaseId().toString() + "-----------------------------------------\n");

	}

	@Override
	public void afterPhase(PhaseEvent e) {
		log.info("\n----------------------------------------------------------------------------------------------------------------\n\n\n");

	}

	@Override
	public PhaseId getPhaseId() {
		return PhaseId.ANY_PHASE;
	}

}
