package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.DemographicService;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Demographic service implementation.
 */
@Service
public class DemographicServiceImpl implements DemographicService {
    private final DemographicDAO demographicDAO;

    public DemographicServiceImpl(final DemographicDAO demographicDAO) {
        this.demographicDAO = demographicDAO;
    }

    @Override
    public Result queryData(final DemographicQuery query) {
        checkArgument(query != null, "query cannot be null.");

        final List<Object> demographicIds = this.demographicDAO.getIds(query);

        final Map<String, Object> summary = ImmutableMap.of(
                PersistenceConstant.Demographic.ID, demographicIds,
                ApplicationConstant.Summary.RECORD_COUNT, demographicIds.size()
        );

        return new Result(Collections.emptyList(), summary);
    }
}
