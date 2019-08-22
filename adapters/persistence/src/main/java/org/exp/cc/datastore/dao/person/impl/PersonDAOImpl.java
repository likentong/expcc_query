package org.exp.cc.datastore.dao.person.impl;

import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Person DAO implementation.
 */
@Repository
public class PersonDAOImpl implements PersonDAO {
    private final DataStoreDAO dataStoreDAO;

    public PersonDAOImpl(final DataStoreDAO dataStoreDAO) {
        this.dataStoreDAO = dataStoreDAO;
    }

    @Override
    public List<Map<String, Object>> getPersonCountById(final Set<Object> id) {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<Map<String, Object>> getPersonById(final Set<Object> id) {
        return this.dataStoreDAO.queryData(
                PersistenceConstant.Entity.PERSON,
                String.format("{ %s : { $in : [%s] }}", PersistenceConstant.Demographic.ID,
                        id.stream()
                                .map(e -> (String) e)
                                .collect(Collectors.joining(","))
                ), Collections.emptyList());
    }
}
