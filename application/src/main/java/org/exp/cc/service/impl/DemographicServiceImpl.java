package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.DemographicService;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Demographic service implementation.
 */
@Service
public class DemographicServiceImpl implements DemographicService {
    private final DemographicDAO demographicDAO;
    private final PersonDAO personDAO;

    public DemographicServiceImpl(final DemographicDAO demographicDAO,
                                  final PersonDAO personDAO) {
        this.demographicDAO = demographicDAO;
        this.personDAO = personDAO;
    }

    @Override
    public Result getIDWithPersonCount(final DemographicQuery query) {
        checkArgument(query != null, "demographic query cannot be null.");

        final List<Object> demographicIds = this.demographicDAO.getIds(query);
        final List<Map<String, Object>> personCountByDemographicID = this.personDAO.getPersonCountByDemographicId(new HashSet<>(demographicIds));

        final Map<String, Object> summary = ImmutableMap.of(
                PersistenceConstant.Demographic.ID, demographicIds,
                ApplicationConstant.Summary.RECORD_COUNT, demographicIds.size()
        );

        return new Result(personCountByDemographicID, summary);
    }
}
