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
        List<Map<String, Object>> persons = this.personDAO.getPersonById(id);

        final Map<String, Object> summary = ImmutableMap.of(
                ApplicationConstant.Summary.RECORD_COUNT, persons.size()
        );

        return new Result(persons, summary);
    }
}
