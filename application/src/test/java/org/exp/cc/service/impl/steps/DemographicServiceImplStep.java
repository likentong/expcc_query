package org.exp.cc.service.impl.steps;

import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.DemographicService;
import org.exp.cc.service.impl.DemographicServiceImpl;
import org.exp.cc.test.ThrowableStep;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Demographic service implementation step.
 */
public class DemographicServiceImplStep extends ThrowableStep<DemographicServiceImplStep> {
    private final DemographicDAO demographicDAO;
    private final PersonDAO personDAO;
    private final DemographicService demographicService;
    private Result result;

    public DemographicServiceImplStep() {
        this.demographicDAO = mock(DemographicDAO.class);
        this.personDAO = mock(PersonDAO.class);
        this.demographicService = new DemographicServiceImpl(this.demographicDAO, this.personDAO);
    }

    /**
     * Step with no setup.
     * @return this instance
     */
    public DemographicServiceImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to setup DemographicDAO getIds.
     * @param query demographic query
     * @param data data
     * @return this instance
     */
    public DemographicServiceImplStep givenISetupDemographicDAOGetIds(final DemographicQuery query, final List<Object> data) {
        when(this.demographicDAO.getIds(query)).thenReturn(data);
        return this;
    }

    /**
     * Step to setup PersonDAO getPersonCountByDemographicId.
     * @param id id
     * @param data data
     * @return this instance
     */
    public DemographicServiceImplStep givenISetupPersonDAOGetPersonCountByDemographicId(final Set<Object> id, List<Map<String, Object>> data) {
        when(this.personDAO.getPersonCountByDemographicId(id)).thenReturn(data);
        return this;
    }

    /**
     * Step to call demographicService getIDWithPersonCount.
     * @param query demographic query
     * @return this instance
     */
    public DemographicServiceImplStep whenIGetIDWithPersonCount(final DemographicQuery query) {
        this.result = this.demographicService.getIDWithPersonCount(query);
        return this;
    }

    /**
     * Step to assert expected result.
     * @param result result
     * @return this instance
     */
    public DemographicServiceImplStep thenResultShouldBe(final Result result) {
        assertThat(this.result).isEqualTo(result);
        return this;
    }
}
