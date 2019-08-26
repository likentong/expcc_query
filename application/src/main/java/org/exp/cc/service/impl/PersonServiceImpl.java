package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.MessageQueue;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Person service impl.
 */
@Service
public class PersonServiceImpl implements PersonService {
    private static final String ID_EXCEPTION_MESSAGE = "id cannot be null or empty.";
    private final PersonDAO personDAO;
    private final MessageQueue messageQueue;

    public PersonServiceImpl(final PersonDAO personDAO, final MessageQueue messageQueue) {
        this.personDAO = personDAO;
        this.messageQueue = messageQueue;
    }

    @Override
    public Result getPerson(final Set<Object> id) {
        checkArgument(!CollectionUtils.isEmpty(id), ID_EXCEPTION_MESSAGE);

        List<Map<String, Object>> persons = this.personDAO.getPersonByDemographicId(id);

        return new Result(persons, generateSummary(persons.size()));
    }

    @Override
    public Result getPersonCountGroupByDemographicID(final Set<Object> id) {
        checkArgument(!CollectionUtils.isEmpty(id), ID_EXCEPTION_MESSAGE);

        List<Map<String, Object>> personCount = this.personDAO.getPersonCountByDemographicId(id);

        return new Result(personCount, generateSummary(personCount.size()));
    }

    @Override
    public void sendToPersonQueue(final Map<String, Object> person) {
        checkArgument(!CollectionUtils.isEmpty(person), "person cannot be null or empty.");

        this.messageQueue.sendMessage(person);
    }

    private Map<String, Object> generateSummary(final Integer recordCount) {
        return ImmutableMap.of(
                ApplicationConstant.Summary.RECORD_COUNT, recordCount
        );
    }
}
