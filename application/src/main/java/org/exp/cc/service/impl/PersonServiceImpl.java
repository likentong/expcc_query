package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.datastore.dao.person.PersonDAO;
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

    public PersonServiceImpl(final PersonDAO personDAO) {
        this.personDAO = personDAO;
    }

    @Override
    public Result getPerson(final Set<Object> id) {
        checkArgument(id != null, "id cannot be null.");

        List<Map<String, Object>> persons = this.personDAO.getPersonById(id);

        return new Result(persons, generateSummary(persons.size()));
    }

    @Override
    public Result getPersonCountGroupByDemographicID(final Set<Object> id) {
        checkArgument(id != null, "id cannot be null.");

        List<Map<String, Object>> personCount = this.personDAO.getPersonCountById(id);

        return new Result(personCount, generateSummary(personCount.size()));
    }

    private Map<String, Object> generateSummary(final Integer recordCount) {
        return ImmutableMap.of(
                ApplicationConstant.Summary.RECORD_COUNT, recordCount
        );
    }
}
