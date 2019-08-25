package org.exp.cc.datastore.dao.demographic.impl.steps;

import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.demographic.DemographicDAO;
import org.exp.cc.datastore.dao.demographic.impl.DemographicDAOImpl;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.test.ThrowableStep;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * DemographicDAO implementation step.
 */
public class DemographicDAOImplStep extends ThrowableStep<DemographicDAOImplStep> {
    private DataStoreDAO dataStoreDAO;
    private DemographicDAO demographicDAO;
    private List<Object> results;

    public DemographicDAOImplStep() {
        this.dataStoreDAO = mock(DataStoreDAO.class);
    }

    /**
     * Step to setup DemographicDAOImpl.
     * @return this instance
     */
    public DemographicDAOImplStep givenISetupDemographicDAOImpl() {
        this.demographicDAO = new DemographicDAOImpl(this.dataStoreDAO);
        return this;
    }

    /**
     * Step to mock DataStoreDAO queryData to return data store results.
     * @param dataStoreResults data store results
     * @return this instance
     */
    public DemographicDAOImplStep givenIMockDataStoreDAOQueryData(final List<Map<String, Object>> dataStoreResults) {
        when(this.dataStoreDAO.queryData(
                eq(PersistenceConstant.Entity.DEMOGRAPHIC),
                any(QueryCriteria.class),
                eq(Collections.singletonList(PersistenceConstant.Demographic.ID)))).thenReturn(dataStoreResults);
        return this;
    }

    /**
     * Step to call DemographicDAOImpl getIds.
     * @param queryCriteria query criteria
     * @return this instance
     */
    public DemographicDAOImplStep whenIGetIds(final QueryCriteria queryCriteria) {
        this.results = this.demographicDAO.getIds(queryCriteria);
        return this;
    }

    /**
     * Step to assert DemographicDAOImpl getIds results.
     * @param results results
     * @return this instance
     */
    public DemographicDAOImplStep thenResultsShouldBe(final List<Object> results) {
        assertThat(this.results).isEqualTo(results);
        return this;
    }

}
