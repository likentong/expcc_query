package org.exp.cc.service.impl.steps;

import org.exp.cc.MessageQueue;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.exp.cc.service.impl.PersonServiceImpl;
import org.exp.cc.test.ThrowableStep;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

/**
 * Person service implementation step.
 */
public class PersonServiceImplStep extends ThrowableStep<PersonServiceImplStep> {
    private final PersonDAO personDAO;
    private final PersonService personService;
    private final MessageQueue messageQueue;
    private Result result;
    private Map<String, Object> person;

    public PersonServiceImplStep() {
        this.personDAO = mock(PersonDAO.class);
        this.messageQueue = mock(MessageQueue.class);

        this.personService = new PersonServiceImpl(this.personDAO, this.messageQueue);
    }

    /**
     * Step with no setup.
     * @return this instance
     */
    public PersonServiceImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to setup PersonDAO getPersonByDemographicId.
     * @param id demographic id
     * @param data data
     * @return this instance
     */
    public PersonServiceImplStep givenISetupPersonDAOGetPersonByDemographicId(final Set<Object> id, final List<Map<String, Object>> data) {
        when(this.personDAO.getPersonByDemographicId(id)).thenReturn(data);
        return this;
    }

    /**
     * Step to setup PersonDAO getPersonCountByDemographicId.
     * @param id demographic id
     * @param data data
     * @return this instance
     */
    public PersonServiceImplStep givenISetupPersonDAOGetPersonCountByDemographicId(final Set<Object> id, final List<Map<String, Object>> data) {
        when(this.personDAO.getPersonCountByDemographicId(id)).thenReturn(data);
        return this;
    }

    public PersonServiceImplStep whenIGetPerson(final Set<Object> id) {
        this.result = this.personService.getPerson(id);
        return this;
    }

    /**
     * Step to call personService getPersonCountGroupByDemographicId.
     * @param id id
     * @return this instance
     */
    public PersonServiceImplStep whenIGetPersonCountByDemographicId(final Set<Object> id) {
        this.result = this.personService.getPersonCountGroupByDemographicID(id);
        return this;
    }

    /**
     * Step to call personService sendToPersonQueue
     * @param person person
     * @return this instance
     */
    public PersonServiceImplStep whenISendToPersonQueue(final Map<String, Object> person) {
        this.person = person;
        this.personService.sendToPersonQueue(person);
        return this;
    }

    /**
     * Step to assert expected result.
     * @param result expected result
     * @return this instance
     */
    public PersonServiceImplStep thenResultShouldBe(final Result result) {
        assertThat(this.result).isEqualTo(result);
        return this;
    }

    /**
     * Step to verify messageQueue sendMessage is called.
     * @return this instance
     */
    public PersonServiceImplStep thenPersonMessageQueueSendMessageIsCalled() {
        verify(this.messageQueue).sendMessage(this.person);
        return this;
    }
}
