package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.impl.amqp.PersonMessageQueue;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Person service impl.
 */
@Service
public class PersonServiceImpl implements PersonService {
    private final PersonDAO personDAO;
    private final PersonMessageQueue personMessageQueue;

    public PersonServiceImpl(final PersonDAO personDAO, final PersonMessageQueue personMessageQueue) {
        this.personDAO = personDAO;
        this.personMessageQueue = personMessageQueue;
    }

    @Override
    public Result getPerson(final Set<Object> id) {
        checkArgument(id != null, "id cannot be null.");

        List<Map<String, Object>> persons = this.personDAO.getPersonByDemographicId(id);

        return new Result(persons, generateSummary(persons.size()));
    }

    @Override
    public Result getPersonCountGroupByDemographicID(final Set<Object> id) {
        checkArgument(id != null, "id cannot be null.");

        List<Map<String, Object>> personCount = this.personDAO.getPersonCountByDemographicId(id);

        return new Result(personCount, generateSummary(personCount.size()));
    }

    @Override
    public void sendToPersonQueue(final Map<String, Object> person) {
        checkArgument(person != null, "person cannot be null.");

        this.personMessageQueue.sendMessage(person);
    }

    private Map<String, Object> generateSummary(final Integer recordCount) {
        return ImmutableMap.of(
                ApplicationConstant.Summary.RECORD_COUNT, recordCount
        );
    }
}
