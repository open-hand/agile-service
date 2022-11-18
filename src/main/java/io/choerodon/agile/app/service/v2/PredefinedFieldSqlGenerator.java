package io.choerodon.agile.app.service.v2;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.search.Condition;

import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-18
 */
public interface PredefinedFieldSqlGenerator {

    String parseSql(FieldTableVO fieldTable,
                    Condition condition,
                    Set<Long> projectIds,
                    List<? extends Object> values,
                    Pair<String, String> dataPair,
                    boolean isSelector);
}
